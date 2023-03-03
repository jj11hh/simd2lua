local Parser = require("parser")
local pprint = require("pprint")

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

local IRGen = {}
function IRGen.__index(t, k) 
    return IRGen[k] 
end

local function new_block(self)
    local blocks = self.blocks
    local idx = #blocks + 1

    local block = { 
        definations = {}, 
        last_definations = {}, 
        incomplete_phis = {},
        preds = {},
        succs = {},
        index = idx,
    }
    
    blocks[idx] = block
    return block
end

function IRGen.new()
    local self = {
        blocks = {},
        user_registry = {}
    }
    
    self.current_block = new_block(self)

    setmetatable(self, IRGen)
    return self
end

local function set_user_(self, block_use, index_use, block_user, index_user, v)
    local registry = self.user_registry
    local key = block_use..","..index_use
    local value = block_user..","..index_user
    local list = registry[key]
    if list == nil then list = {} end
    list[value] = v
    registry[key] = list
end

local function add_user(self, use, user)
    if type(use) ~= "table" then return end
    local block_use = use[3].index
    local index_use = use[4]

    local block_user = user[3].index
    local index_user = user[4]

    set_user_(self, block_use, index_use, block_user, index_user, true)
end

local function remove_user(self, use, user)
    if type(use) ~= "table" then return end
    local block_use = use[3].index
    local index_use = use[4]

    local block_user = user[3].index
    local index_user = user[4]
    
    set_user_(self, block_use, index_use, block_user, index_user, nil)
end

local string_find = string.find
local string_sub = string.sub

local function get_users(self, def)
    local registry = self.user_registry
    local blocks = self.blocks
    local users = {}
    local block_use = def[3].index
    local index_use = def[4]
    local key = block_use..","..index_use
    local value = registry[key]
    if value == nil then
        return users
    end
    
    for item,v in pairs(value) do
        assert(v == true)
        local sep = string_find(item, ",")
        local block_index = tonumber(string_sub(item, 1, sep-1))
        local index = tonumber(string_sub(item, sep+1, #item))
        users[#users+1] = blocks[block_index].definations[index]
    end
    
    return users
end

local function replace_in_users(self, origin, users, replacement)
    for i=1,#users do
        local user = users[i]
        local val = user[2]
        for j=2,#val do
            if val[j] == origin then -- Is this safe?
                val[j] = replacement
            end
        end
        remove_user(self, origin, user) -- Remove user from origin
    end
end

local function write_variable(self, var, block, value)
    if block == nil then block = self.current_block end
    local defs = block.definations
    local last_defs = block.last_definations
    local def_idx = #defs+1
    local def
    if type(value) == "table" and value[4] == 0 then -- Sattle Down an Isolated Value
        def = value
        def[1] = var
        def[3] = block
        def[4] = def_idx
    else
        def = {var, value, block, def_idx}
    end
    defs[def_idx] = def
    last_defs[var] = def_idx
    return def
end

local function create_isolated(self, var, block, value)
    return {var, value, block, 0}
end

local add_phi_operands

local function read_variable(self, var, block)
    if block == nil then block = self.current_block end
    local defs = block.definations
    local idx = block.last_definations[var]
    local incomplete_phis = block.incomplete_phis
    if idx ~= nil then return defs[idx] end
    local value
    -- not found in current block
    if not block.sealed then -- not sealed, may have more predecessors
        value = create_isolated(self, var, block, {"phi"}) -- place a empty phi
        incomplete_phis[var] = value
    elseif #block.preds == 1 then
        value = read_variable(self, var, block.preds[1])
    else
        value = create_isolated(self, var, block, {"phi"})
        local def = write_variable(self, var, block, value)
        add_phi_operands(self, def)
    end
    local def = write_variable(self, var, block, value)
    return def
end

local function is_phi(val)
    return type(val) == "table" and val[1] == "phi"
end

local function try_remove_trivial_phi(self, phi_def)
    local phi = phi_def[2]
    local block = phi_def[3]
    local same = nil
    for i=2,#phi do
        operand = phi[i]
        if same == operand or (type(same) == "table" and same[2] == phi) then
            goto continue
        end
        if same ~= nil then
            return phi
        end
        same = operand
        ::continue::
    end
    
    if same == nil then
        same = {phi_def[1], nil, nil, -1}
    end
    
    local users = get_users(self, phi_def)
    replace_in_users(self, phi_def, users, same)
    
    for i=1,#users do
        local user = users[i]
        if user ~= phi_def then
            if is_phi(user[2]) then
                try_remove_trivial_phi(self, user)
            end
        end
    end
    
    return same
end

add_phi_operands = function(self, phi_def, variable)
    local phi = phi_def[2]
    local block = phi_def[3]
    local preds = block.preds

    for i=1,#preds do
        local pred = preds[i]
        local val = read_variable(self, variable, pred)
        add_user(self, val, phi_def)
        phi[#phi+1] = val
    end
    
    return try_remove_trivial_phi(self, phi_def)
end

local function seal_block(self, block)
    local phis = block.incomplete_phis
    for k,v in pairs(phis) do
        add_phi_operands(self, v, k)
    end
    block.incomplete_phis = {}
    block.sealed = true
end

function IRGen.complete(self)
    seal_block(self, self.current_block)
end

function IRGen.assign(self, dest, src)
    local op, operand0, operand1
    if type(src) == "table" then
        op = src[1]
        operand0 = src[2]
        operand1 = src[3]
    else -- Move instruction
        op = "="
        operand0 = src
    end

    if type(operand0) == "string" then
        operand0 = read_variable(self, operand0)
    end
    if type(operand1) == "string" then
        operand1 = read_variable(self, operand1)
    end
    
    local def = write_variable(self, dest, nil, {op, operand0, operand1})

    if type(operand1) == "table" then
        add_user(self, operand0, def)
    end

    if type(operand1) == "table" then
        add_user(self, operand1, def)
    end
end

function IRGen.binary(self, op, a, b)
    return {op, a, b}
end

function IRGen.unary(self, op, a)
    return {op, a}
end

function IRGen.identifier(self, name)
    return name
end

function IRGen.number_literal(self, value)
    return value
end

function IRGen.bool_literal(self, value)
    return value
end

function Optimizer.optimize(self)

end

local Compiler = require("simd2lua")

local function def2str(def)
    if type(def) ~= "table" then return tostring(def) end
    local name, val, block, index = table.unpack(def)
    local block_idx
    if block == nil then
        block_idx = -1
    else
        block_idx = block.index
    end
    return table.concat{name, "[", block_idx, ",", index, "]"}
end

local function print_ir(def)
    local val = def[2]
    local block = def[3]
    local idx = def[4]
    local str = {def[1], "[", block.index, ",", idx, "] = [", val[1], "]("}
    if #val > 1 then
        for i=2,#val do
            str[#str+1] = def2str(val[i])
            str[#str+1] = ", "
        end
        str[#str] = ")"
    else
        str[#str+1] = ")"
    end
    print(table.concat(str))
end

local function test()
    local code = [[
        function testfunc(a :int, b :int) :int
            local t:int = a*a + b*b
            t = t+3
            return a + b * 3 + t
        end
    ]]
    
    local result = Compiler.compile(code)
    local luacode = table.concat(result.functions["testfunc"].code)
    print(luacode)

    local irgen = IRGen.new()
    local parser = Parser.new(luacode, irgen)
    Parser.parse(parser)
    irgen:complete()
    
    local defs = irgen.current_block.definations
    for i=1,#defs do
        print_ir(defs[i])
    end
end

test()