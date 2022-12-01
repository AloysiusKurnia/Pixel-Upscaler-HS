interface HQXCase {
    cases: number[];
    /** 
     * Array of either 4 elements for HQ2X or 9 elements for HQ3X.
     */
    blendTypes: Record<string, BlendType>;
}

interface ConditionalBlend {
    differenceToCheck: [number, number];
    ifTrue: string;
    ifFalse: string;
}

type BlendType = string | ConditionalBlend;

interface ActiveConditional {
    differenceToCheck: [number, number];
    isTrue: boolean;
    blendTypesIfTrue: Record<string, string>;
    blendTypesIfFalse: Record<string, string>;
}

function parsePatterns(text: string) {
    const lines = text.split('\n');
    const cases: HQXCase[] = [];
    let currentCase: HQXCase = { cases: [], blendTypes: {} };
    let activeConditional: ActiveConditional | null = null;
    for (let line of lines) {
        line = line.trim();
        let match = line.match(/case (\d+):/);
        if (match) {
            currentCase.cases.push(parseInt(match[1]));
            continue;
        }
        match = line.match(/if \(Diff\(w\[(\d)\], w\[(\d)\]\)\)/);
        if (match) {
            const [_, a, b] = match;
            activeConditional = {
                differenceToCheck: [parseInt(a), parseInt(b)],
                blendTypesIfTrue: {},
                blendTypesIfFalse: {},
                isTrue: true,
            };
            continue;
        }
        match = line.match('else');
        if (match) {
            activeConditional!.isTrue = false;
            continue;
        }
        match = line.match('}');
        if (match) {
            if (activeConditional && !activeConditional.isTrue) {
                for (const key in activeConditional.blendTypesIfFalse) {
                    currentCase.blendTypes[key] = {
                        differenceToCheck: activeConditional.differenceToCheck,
                        ifTrue: activeConditional.blendTypesIfTrue[key],
                        ifFalse: activeConditional.blendTypesIfFalse[key],
                    };
                }

                activeConditional = null;
            }
            continue;
        }
        match = line.match(/PIXEL(\d\d)_(.+)/);
        if (match) {
            const [_, pattern, blendType] = match;

            if (activeConditional) {
                if (activeConditional.isTrue) {
                    activeConditional.blendTypesIfTrue[pattern] = blendType;
                } else {
                    activeConditional.blendTypesIfFalse[pattern] = blendType;
                }
            } else {
                currentCase.blendTypes[pattern] = blendType;
            }
            continue;
        }
        match = line.match('break;');
        if (match) {
            cases.push(currentCase);
            currentCase = { cases: [], blendTypes: {} };
            continue;
        }
        match = line.match(/PIXEL(\d\d)/);
        if (match) {
            const [_, pattern] = match;
            if (activeConditional) {
                if (activeConditional.isTrue) {
                    activeConditional.blendTypesIfTrue[pattern] = '';
                } else {
                    activeConditional.blendTypesIfFalse[pattern] = '';
                }
            } else {
                currentCase.blendTypes[pattern] = '';
            }
            continue;
        }
    }
    return cases;
}

function transpilePatterns(cases: HQXCase[], variant: 'HQ2x' | 'HQ3x') {
    const variantNumber = variant === 'HQ2x' ? 2 : 3;
    let out = `module Src.Algorithms.HQXPatterns.${variant} where\n\n`;
    out += `import Src.Algorithms.HQX\n`;
    out += `import Src.Algorithms.Common\n\n`;

    out += `getPixelValue${variant} :: Square3x3 RGBPixel -> Square${variant == 'HQ2x' ? '2x2' : '3x3'} RGBPixel\n`;
    out += `getPixelValue${variant} neighborhood\n`;
    const patterns = variant === 'HQ2x'
        ? ['00', '01', '10', '11']
        : ['00', '01', '02', '10', '11', '12', '20', '21', '22'];
    for (const c of cases) {
        if (c.cases.length === 1) {
            out += `  | pattern == ${c.cases[0]} = (\n`;
        } else {
            out += `  | pattern \`elem\` [${c.cases.join(', ')}] = (\n`;
        }
        for (const pattern of patterns) {
            const blendType = c.blendTypes[pattern];
            if (typeof blendType === 'string') {
                out += `      blend${variant} neighborhood P${variantNumber}_${pattern} B${variantNumber}_${blendType}`;
            } else {
                const [a, b] = blendType.differenceToCheck;
                out += `      if isDiff neigborhood ${a} ${b}\n`;
                out += `        then blend${variant} neighborhood P${variantNumber}_${pattern} B${variantNumber}_${blendType.ifTrue}\n`;
                out += `        else blend${variant} neighborhood P${variantNumber}_${pattern} B${variantNumber}_${blendType.ifFalse}`;
            }
            if (pattern === '11') {
                out += ')\n';
            } else {
                out += ',\n';
            }
        }
    }
    out += '  where pattern = hq2xGetPattern neighborhood\n';
    return out;
}

function transpileBlendType(text: string, variant: 'HQ2x' | 'HQ3x') {
    const lines = text.split('\n');
    const n = variant === 'HQ2x' ? 2 : 3;
    const blendTypes: {
        posInsideNeighbor: string,
        interpType: string, 
        pattern: string,
        c1: string,
        c2: string | null,
        c3: string | null,
    }[] = [];
    const patterns = new Set<string>();
    const neighborhoodPositions = new Set<string>();
    for (let line of lines) {
        line = line.trim();
        
        if (line === '#define PIXEL11     *(dp+dpL+1) = w[5];') {
            console.log('found');
            
            patterns.add(`B${n}_`);
            neighborhoodPositions.add(`P${n}_11`);
            blendTypes.push({ posInsideNeighbor: '11', pattern: '', interpType: 'singleton', c1: '5', c2: null, c3: null });
            continue;
        }
        let match = line.match(/#define PIXEL(\d\d)_([^ ]+) +Interp(\d)\([dpL+12]+, w\[(\d)\], w\[(\d)\]\);/);
        if (match) {
            const [_, posInsideNeighbor, pattern, interpType, c1, c2] = match;
            patterns.add(`B${n}_${pattern}`);
            neighborhoodPositions.add(`P${n}_${posInsideNeighbor}`);
            blendTypes.push({ posInsideNeighbor, pattern, interpType, c1, c2, c3: null });
            continue;
        }
        match = line.match(/#define PIXEL(\d\d)_([^ ]+) +\*[()dpL+12]+ += w\[5\];/);
        if (match) {
            const [_, posInsideNeighbor, pattern] = match;
            patterns.add(`B${n}_${pattern}`);
            neighborhoodPositions.add(`P${n}_${posInsideNeighbor}`);
            blendTypes.push({ posInsideNeighbor, pattern, interpType: 'singleton', c1: '5', c2: null, c3: null });
            continue;
        }
        match = line.match(/#define PIXEL(\d\d)_([^ ]+) +Interp(\d)\([dpL+12]+, w\[(\d)\], w\[(\d)\], w\[(\d)\]\);/);
        if (match) {
            const [_, posInsideNeighbor, pattern, interpType, c1, c2, c3] = match;
            patterns.add(`B${n}_${pattern}`);
            neighborhoodPositions.add(`P${n}_${posInsideNeighbor}`);
            blendTypes.push({ posInsideNeighbor, pattern, interpType, c1, c2, c3 });
            continue;
        }
    }
    let out = `type PositionPattern${variant} = ${Array.from(neighborhoodPositions).join(' | ')}\n\n`;
    out += `type BlendType${variant} = ${Array.from(patterns).join(' | ')}\n\n`;
    out += `blend${variant} :: PositionPattern${variant} -> BlendType${variant} -> Square3x3 RGBPixel -> RGBPixel\n`;
    for (const blendType of blendTypes) {
        out += `blend${variant} neighborhood `
        out += `P${n}_${blendType.posInsideNeighbor} `
        out += `B${n}_${blendType.pattern} = `
        if (blendType.interpType === 'singleton') {
            out += `getNthColorFromNeighborhood 5 neighborhood\n`;
        } else {
            out += `blendHQX neighborhood ${blendType.pattern} ${blendType.c1} ${blendType.c2} ${blendType.c3 ?? '5'}\n`;
        }
    }
    return out;
}

async function processVariant(variant: 'HQ2x' | 'HQ3x') {
    const text = await Deno.readTextFile(`other_source/${variant.toLowerCase()}.txt`);
    const cases = parsePatterns(text);
    const out = transpilePatterns(cases, variant);
    await Deno.writeTextFile(`src/Algorithms/HQXPatterns/${variant}.hs`, out);
}

async function main() {
    const text = await Deno.readTextFile(`other_source/hq3xBlend.txt`);
    console.log(transpileBlendType(text, 'HQ3x'));

}

main();