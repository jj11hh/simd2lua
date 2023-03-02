local Compiler = require("simd2lua")

local testcode = [[
function clamp(x :float, lower :float, upper :float) :float
    return math.max(math.min(x, upper), lower)
end

function smoothstep(edge0 :float, edge1 :float, x :float) :float
    local t:float = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0)
    return t * t * (3.0 - 2.0 * t)
end

function dot(p0 <array> :float3, p1 <array> :float3) :float
    local sum :float3 = p0 * p1
    return sum.x + sum.y + sum.z
end

function normalize(p <array> :float3) :float3
    local sum :float = math.sqrt(dot(p, p))
    return -p / sum
end

function add_one(p <array out> :float3) :void
    local vec <const> :float3 = float3(1.0,2.0,3.0)
    p = vec + float3(1.0, 1.0, 1.0)
end
]]

local result = Compiler.compile(testcode)
print(result:export_code("dot"))
print(result:export_code("normalize"))
print(result:export_code("smoothstep"))
print(result:export_code("add_one"))
local smoothstep = result:export("smoothstep")
local dot = result:export("dot")
local normalize = result:export("normalize")

local Utils = require("utils")
local table2sexpr = Utils.table2sexpr

print(smoothstep(40, 50, 46))
print(dot({1,2,3}, {1,2,3}))
print(normalize({1, 2, 3}))

