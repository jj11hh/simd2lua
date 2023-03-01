local Parser = require("parser")

local Optimizer = {
    __index = function(t, k) return Optimizer[k] end
}

-- Code: list of statements
-- Returns: Registers need be read from caller, including all inout or out parameters
function Optimizer.new(code, returns)
    local self = {
        source = code,
        returns = returns,
        versions = {},
        blocks = {},
        edges = {},
    }
    setmetatable(self, Optimizer)
    return self
end

function new_version(self, name)
    local versions = self.versions
    local ver = versions[name]
    if ver == nil then
        versions[name] = 0
        return 0
    end
    versions[name] = ver + 1
    return ver + 1
end

function Optimizer.optimize(self)
end
