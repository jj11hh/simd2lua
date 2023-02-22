-- Build AST, do function inlining, constant folding, etc..

local Utils = require("utils")
local table2sexpr = Utils.table2sexpr
local table_find = Utils.table_find
local table_concat = table.concat
local string_find = string.find

local Codegen = {}

local floor = math.floor
local ceil = math.ceil

local builtin_signatures = {
    ["math.abs"] = { {"int", "int"}, {"float", "float"} },
    ["math.acos"] = { {"float", "float"} },
    ["math.asin"] = { {"float", "float"} },
    ["math.atan"] = { {"float", "float"} },
    ["math.atan2"] = { {"float", "float", "float"} },
    ["math.ceil"] = { {"float", "float"} },
    ["math.cos"] = { {"float", "float"} },
    ["math.cosh"] = { {"float", "float"} },
    ["math.deg"] = { {"float", "float"} },
    ["math.exp"] = { {"float", "float"} },
    ["math.floor"] = { {"float", "float"} },
    ["math.fmod"] = { {"float", "float", "float"} },
    ["math.log"] = { {"float", "float"} },
    ["math.log10"] = { {"float", "float"} },
    ["math.max"] = { {"int", "int", "int"}, {"float", "float", "float"} },
    ["math.min"] = { {"int", "int", "int"}, {"float", "float", "float"} },
    ["math.pow"] = { {"float", "float", "float"} },
    ["math.rad"] = { {"float", "float"} },
    ["math.sin"] = { {"float", "float"} },
    ["math.sinh"] = { {"float", "float"} },
    ["math.sqrt"] = { {"float", "float"} },
    ["math.tan"] = { {"float", "float"} },
    ["math.tanh"] = { {"float", "float"} },
    ["int"] = { {"int", "int"}, {"int", "float"} },
    ["float"] = { {"float", "int"}, {"float", "float"} },
}

local function new_reg(self, tag, dtype)
    local name = dtype .. "_" .. tag .. "_" .. self.regid
    self.regid = self.regid + 1
    return name
end

local function emit(self, code)
    if code == "" then return end
    local linestop = ""
    if string.sub(code, #code, #code) ~= "\n" then
        linestop = "\n"
    end
    local buf = self.code
    buf[#buf+1] = code .. linestop
end

local function emit_finit(self, code)
    if code == "" then return end
    local linestop = ""
    if string.sub(code, #code, #code) ~= "\n" then
        linestop = "\n"
    end
    local buf = self.function_init
    buf[#buf+1] = code .. linestop
end

local function use_feature(self, mod, feature, dtype)
    local feature_name = mod .. "." .. feature
    if self.used_features[feature_name] ~= nil then 
        return self.used_features[feature_name] 
    end

    local reg = new_reg(self, feature, dtype)

    local impl = _G[mod][feature]

    if type(impl) == "number" then
        assert(dtype == "float" or dtype == "int")
        return {type=dtype, value=impl, code=""}
    end

    assert(type(impl) == "function" and dtype == "builtin")

    local init_code = self.init_code
    init_code[#init_code+1] = "local " .. reg .. "=" .. feature_name .. "\n";
    local retval = {type=dtype, value=reg, code="", impl=impl}

    self.used_features[feature_name] = retval
    return retval
end

local function cast_float_to_int(self, x)
    assert(x.type == "float")
    if type(x.value) == "number" then
        local value
        if x.value > 0 then value = floor(x.value) else value = ceil(x.value) end
        return {type="int", value=value, code=x.code}
    end
    local reg = new_reg(self, "temp", "int")
    local code = {
        x.code, 
        "local ", reg, "\n",
        "if ", x.value, ">0 then ", reg, "=floor(", x.value, ") else ", reg, "=ceil(", x.value, ") end\n"
    }
    return {type="int", value=reg, code=table_concat(code)}
end

local function cast_int_to_float(self, x)
    assert(x.type == "int")
    value = x.value
    if type(value) == "number" then value = value+0.0 end
    return {type="float", value=value, code=x.code}
end

local binfuncs = {
    ["+"] = function(a, b) return a + b end,
    ["-"] = function(a, b) return a - b end,
    ["*"] = function(a, b) return a * b end,
    ["/"] = function(a, b) return a / b end,
    ["//"] = function(a, b) return a // b end,
    ["%"] = function(a, b) return a % b end,
    ["<<"] = function(a, b) return a << b end,
    [">>"] = function(a, b) return a >> b end,
    ["~"] = function(a, b) return a ~ b end,
    ["|"] = function(a, b) return a | b end,
    ["&"] = function(a, b) return a & b end,
}

local compfuncs = {
    ["<"]  = function(a, b) return a <  b end,
    ["<="] = function(a, b) return a <= b end,
    ["=="] = function(a, b) return a == b end,
    [">="] = function(a, b) return a >= b end,
    [">"]  = function(a, b) return a >  b end,
}

function Codegen.binary(self, op, a, b)
    local dtype
    if a.type == "float" or b.type == "float" then
        assert(a.type == "float" or a.type == "int", "cannot perform math between " .. a.type .. " and " .. b.type)
        assert(b.type == "float" or b.type == "int", "cannot perform math between " .. a.type .. " and " .. b.type)
        assert(op ~= "<<" and op ~= ">>" and op ~= "~" and op ~= "|" and op ~= "&", "cannot do bitwise operation on float values")
        if a.type == "int" then a = cast_int_to_float(self, a) end
        if b.type == "int" then b = cast_int_to_float(self, b) end
        dtype = "float"
    elseif a.type == "int" and b.type == "int" then
        dtype = "int"
        if op == "/" then op = "//" end
    else
        error("cannot perform math between " .. a.type .. " and " .. b.type)
    end

    if type(a.value) == "number" and type(b.value) == "number" then -- Constant Folding
        return {type=dtype, value=binfuncs[op](a.value, b.value), code=""}
    end

    local reg = new_reg(self, "temp", dtype)
    emit_finit(self, "local " .. reg)
    local code = {a.code, b.code, reg .. "=" .. a.value .. op .. b.value .. "\n"}
    return {type=dtype, value=reg, code=table_concat(code)}
end

function Codegen.call(self, func, params)
    assert(func.type == "builtin")
    local funcname = func.value
    local funcimpl = func.impl

    local codes = {}
    local args = {}
    local all_constants = true

    for i,param in ipairs(params) do
        codes[i] = param.code
        args[i] = param.value

        if type(param.value) ~= "number" and type(param.value) ~= "boolean" then
            all_constants = false
        end
    end

    local return_type = nil
    local signature = func.signature
    for i=1,#signature do
        local sig = signature[i]
        if #sig-1 == #params then
            local match = true
            for j=2,#sig do
                if params[j-1].type ~= sig[j] then
                    match = false
                end
            end
            if match then
                return_type = sig[1]
            end
        end
    end

    assert(return_type ~= nil, "no matched function " .. tostring(func.name) .. " declared")
    local reg = new_reg(self, "call", return_type) -- TODO: Do type check!
    if all_constants and funcimpl ~= nil then
        return { type="float", value=funcimpl(table.unpack(args)), code="" }
    end

    emit_finit(self, "local " .. reg)
    codes[#codes+1] = table_concat{reg, "=", funcname, "(", table.concat(args, ","), ")\n"}
    return { type="float", value=reg, code=table_concat(codes) }
end

function Codegen.expr_statement(self, expr)
    emit(self, expr.code)
end

local function try_cast(self, var, dtype)
    if var.type == "int" and dtype == "float" then
        return cast_int_to_float(self, var)
    end
end

function Codegen.decl_variable(self, name, attributes, dtype, init)
    local locals = self.locals
    local f_init = self.function_init
    local is_const = table_find(attributes, "const") ~= nil

    if is_const and init == nil then
        error("constant variable " .. name .. " has no initial value")
    end

    if init ~= nil and init.type ~= dtype then
        init = try_cast(self, init, dtype)
        if init == nil then
            error("type mismatch for variable " .. name)
        end
    end

    local reg = new_reg(self, name, dtype)
    if is_const then
        if type(init.value) == "number" or type(init.value) == "boolean" then -- Assign Constant to it
            locals[name] = init
            return
        end
        emit_finit(self, "local " .. reg)
        this = {type=dtype, value=reg, lvalue=reg, code=""}
        Codegen.assign(self, this, init)
        this.lvalue = nil -- Make this const
        locals[name] = this
        return
    end

    if init == nil then
        init = {type=dtype, value=0, code=""}
    end

    emit_finit(self, "local " .. reg)
    this = {type=dtype, value=reg, lvalue=reg, code=""}
    Codegen.assign(self, this, init)
    locals[name] = this
end

function Codegen.assign(self, dest, src)
    if dest.type == "float" and src.type == "int" then
        src = cast_int_to_float(self, src)
    end
    assert(dest.type == src.type, "type mismatch")
    assert(dest.lvalue ~= nil, "cannot assign to a constant value")
    emit(self, src.code)
    emit(self, dest.lvalue .. "=" .. src.value)
end

function Codegen.decl_function(self)
end

function Codegen.number_literal(self, x)
    if string_find(tostring(x), "[e%.]") == nil then
        return {type="int", value=x, code=""}
    else
        return {type="float", value=x, code=""}
    end
end

function Codegen.bool_literal(self, x)
    return {type="bool", value=x, code=""}
end

function Codegen.identifier(self, x)
    if self.locals[x] ~= nil then
        return self.locals[x]
    end

    if self.globals[x] ~= nil then
        return self.globals[x]
    end

    if x == "math" then
        return {type="builtin_module", value=x, code=""}
    end

    error("undefined variable " .. x)
end

function Codegen.get_field(self, x, field)
    if x.type == "builtin_module" then
        if x.value == "math" then
            assert(math[field] ~= nil, "unsupported math function " .. field)
            local dtype
            if type(math[field]) == "number" then dtype = "float" else dtype = "builtin" end
            local func = use_feature(self, "math", field, dtype)

            if dtype == "builtin" then
                local name = "math." .. field
                local signature = builtin_signatures[name]
                assert(signature ~= nil, "unsupported math function " .. field)
                func.signature = signature
                func.name = name
            end

            return func
        end
    end

    error("Not implemented")
end

function Codegen.begin_for(self, name, begin, ends, step)
    assert(begin.type == "int" and ends.type == "int" and step.type == "int", "begin, end and step should be integer value")
    local reg = new_reg(self, name, "int")
    self.locals[name] = {type="int", value=reg, code=""}
    emit(self, begin.code)
    emit(self, ends.code)
    emit(self, step.code)
    emit(self, "for " .. reg .. "=" .. begin.value .. "," .. ends.value .. "," .. step.value .. " do")
end

function Codegen.end_for(self)
    emit(self, "end")
end

function Codegen.__index(t, key)
    return Codegen[key]
end

function Codegen.new()
    local self = {
        regid = 0,
        used_features = {},
        init_code = {},
        function_init = {},
        code = {},
        locals = {},
        globals = {},
    }

    setmetatable(self, Codegen)
    return self
end

local Parser = require("parser")
local function test_codegen()
    local code = [[
        local a :float = 1 + 2 + 3 * 2 + 4 + math.sin(24.0)
        local b <const> :float = a + a * 3 * 5
        local c :float = math.sin(b)
        c = 114514
        local d :int = 1 + 3 * 4
        local e :float = math.min(d, d)

        for i=1,d do
            e = e * 2
        end
    ]]
    local cg = Codegen.new()
    local parser = Parser.new(code, cg, "main")

    local ok, message = pcall(Parser.parse, parser)
    if not ok then
        print("error: " .. message)
        print("file: " .. parser.filename)
        print("line: " .. parser.line)
    else
        print(table.concat(cg.init_code))
        print(table.concat(cg.function_init))
        print(table.concat(cg.code))
    end
end

test_codegen()