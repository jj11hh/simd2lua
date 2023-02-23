-- Build AST, do function inlining, constant folding, etc..

local Utils = require("utils")
local table2sexpr = Utils.table2sexpr
local table_find = Utils.table_find
local table_extend = Utils.table_extend
local table_concat = table.concat
local string_find = string.find
local string_match = string.match
local string_gsub = string.gsub
local string_sub = string.sub

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

local function emit_to_buffer(buf, code, indent)
    if type(code) == "table" then
        for i=1,#code do emit_to_buffer(buf, code[i], indent) end
        return
    end

    if code == "" then return end
    if indent ~= nil then
        code = indent .. code
    end

    local linestop = ""
    if string_sub(code, #code, #code) ~= "\n" then
        linestop = "\n"
    end
    buf[#buf+1] = code .. linestop
end

local function emit_finit(self, code)
    emit_to_buffer(self.function_init, code)
end

local function emit(self, code)
    local level = self.indent
    local char = self.indent_char
    local indent = ""

    for i =1,level do
        indent = indent .. char
    end

    emit_to_buffer(self.code, code, indent)
end

local function fold_code(var)
    local var_code = var.code
    if #var_code == 0 then return end
    local last_code = var_code[#var_code]
    local pattern = "^"..tostring(var.value).."="
    if string_match(last_code, pattern) then
        local substr = string_sub(last_code, #pattern, #last_code)
        var_code[#var_code] = nil
        var.value = string_gsub(substr, "\n", "")
    end
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

    emit_to_buffer(self.init_code, "local " .. reg .. "=" .. feature_name)
    local retval = {type=dtype, value=reg, code="", impl=impl}

    self.used_features[feature_name] = retval
    return retval
end

local function check_signature(sig, params)
    if #sig-1 == #params then
        local match = true
        for j=2,#sig do
            if params[j-1].type ~= sig[j] then match = false end
        end
        if match then return sig[1] end
    end
end

local function cast_float_to_int(self, x)
    assert(x.type == "float")
    if x.constexpr then
        local value
        if x.value > 0 then value = floor(x.value) else value = ceil(x.value) end
        return {type="int", value=value, code=x.code, constexpr=true}
    end
    local reg = new_reg(self, "temp", "int")
    local floor_func = use_feature(self, "math", "floor", "builtin").value
    local ceil_func = use_feature(self, "math", "ceil", "builtin").value
    emit_finit(self, "local " .. reg)
    local code = {}
    table_extend(code, x.code)
    emit_to_buffer(code, "if " .. x.value .. ">0 then ")
    emit_to_buffer(code, reg .. "=" .. floor_func .. "(" .. x.value .. ")")
    emit_to_buffer(code, "else")
    emit_to_buffer(code, reg .. "=" .. ceil_func .. "(" .. x.value .. ")")
    emit_to_buffer(code, "end")
    return {type="int", value=reg, code=code}
end

local function cast_int_to_float(self, x)
    assert(x.type == "int")
    value = x.value
    if type(value) == "number" then value = value+0.0 end
    return {type="float", value=value, code=x.code, constexpr=x.constexpr}
end

local builtin_functions = {}

function builtin_functions.int(self, params)
    assert(#params == 1)
    local input = params[1]
    if input.type == "int" then
        return input
    end
    return cast_float_to_int(self, input)
end

function builtin_functions.float(self, params)
    assert(#params == 1)
    local input = params[1]
    if input.type == "float" then
        return input
    end
    return cast_int_to_float(self, input)
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

local logicfuncs = {
    ["and"] = function(a, b) return a and b end,
    ["or"] = function(a, b) return a or b end,
}

function Codegen.binary(self, op, a, b)
    local dtype

    if logicfuncs[op] ~= nil then
        dtype = "bool"
        func = logicfuncs[op]

        if a.constexpr and b.constexpr then
            return {type=dtype, value=func(a.value, b.value), code="", constexpr=true}
        end

        if op == "and" and (a.constexpr or b.constexpr) then
            if a.constexpr then a, b = b, a end
            if b.value then return a else return b end
        end

        if op == "or" and (a.constexpr or b.constexpr) then
            if a.constexpr then a, b = b, a end
            if b.value then return b else return a end
        end

        local reg = new_reg(self, "temp", dtype)
        emit_finit(self, "local " .. reg)
        local code = {}
        table_extend(code, a.code)
        table_extend(code, b.code)
        emit_to_buffer(code, reg.."="..tostring(a.value).." "..op.." "..tostring(b.value))
        return {type=dtype, value=reg, code=code}
    end

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

    local func = compfuncs[op]
    if func ~= nil then
        dtype = "bool"
    else
        func = binfuncs[op]
    end

    assert(func ~= nil)

    if a.constexpr and b.constexpr then -- Constant Folding
        return {type=dtype, value=func(a.value, b.value), code="", constexpr=true}
    end

    local reg = new_reg(self, "temp", dtype)
    emit_finit(self, "local " .. reg)

    local code = {}
    table_extend(code, a.code)
    table_extend(code, b.code)
    emit_to_buffer(code, reg.."="..tostring(a.value).." "..op.." "..tostring(b.value))

    return {type=dtype, value=reg, code=code}
end

function Codegen.call(self, func, params)
    assert(func.type == "builtin" or func.type == "function")

    -- Process Builtin Functions
    if func.codegen ~= nil then
        return func.codegen(self, params)
    end

    assert(check_signature(func.signature, params) ~= nil, "type mismatch on function call:" .. func.name)

    -- Do Function Inlining
    local return_label = new_reg(self, func.name, "exit")
    local old_label = func.exit_label
    local func_inits = func.init
    local func_params = func.parameters

    -- Copy Registers
    for i,v in ipairs(func_inits) do
        emit_finit(self, v)
    end

    local code = {}

    for i,param in ipairs(params) do
        emit_to_buffer(code, param.code)
    end

    -- Copy Code
    for i,source in ipairs(func.code) do
        for j,param in ipairs(params) do
            -- TODO: Check for inout, out
            source = string_gsub(source, func_params[j].value, param.value)
        end
        source = string_gsub(source, old_label, return_label)
        emit_to_buffer(code, source)
    end

    local return_reg = func.return_reg
    if return_reg == nil then
        return {type="void", code=code, value=""}
    else
        return {type=return_reg.type, code=code, value=return_reg.value}
    end
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
        if init.constexpr then -- Assign Constant to it
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
    fold_code(src)
    if dest.type == "float" and src.type == "int" then
        src = cast_int_to_float(self, src)
    end
    assert(dest.type == src.type, "type mismatch")
    assert(dest.lvalue ~= nil, "cannot assign to a constant value")
    emit(self, src.code)
    emit(self, dest.lvalue .. "=" .. src.value)
end

function Codegen.decl_param(self, name, attributes, dtype)
    return {name, attributes, dtype}
end

function Codegen.decl_function(self, name, params, attributes, dtype)
    self.function_name = name
    self.function_init = {}
    self.code = {}
    self.exit_label = new_reg(self, name, "exit")

    if dtype ~= "void" then
        local output_reg = new_reg(self, name, dtype)
        emit_finit(self, "local "..output_reg)
        self.return_reg = {type=dtype, value=output_reg, code="", lvalue=output_reg}
    else
        self.return_reg = nil
    end

    local signature = {dtype}
    local locals = {}
    local parameters = {}

    -- Caller should assign parameters to these registers
    for i,param in ipairs(params) do
        local pname, pattr, ptype = table.unpack(param)
        local param_reg = new_reg(self, pname, ptype)
        param[4] = param_reg
        emit_finit(self, "local "..param_reg)
        -- By default, all parameters are immutable, excepting they are inout or out
        if table_find(pattr, "inout") or table_find(pattr, "out") then
            locals[pname] = {type=ptype, value=param_reg, code="", lvalue=param_reg}
        else
            locals[pname] = {type=ptype, value=param_reg, code=""}
        end
        parameters[i] = {type=ptype, value=param_reg, code="", attributes=pattr}
        signature[i+1] = ptype
    end

    self.signature = signature
    self.locals = locals
    self.parameters = parameters
end

function Codegen.return_val(self, value)
    local return_reg = self.return_reg
    local exit_label = self.exit_label
    assert(not(return_reg == nil and value ~= nil), "attemping return value from function with void return type")
    assert(not(return_reg ~= nil and value == nil), "return nothing but function type isn't void")

    if value == nil then
        return emit(self, "goto " .. exit_label)
    end

    assert(return_reg.type == value.type, "return type mismatch")

    Codegen.assign(self, return_reg, value)
    return emit(self, "goto " .. exit_label)
end

function Codegen.end_function(self)
    -- Eliminate Empty Goto
    local code_buffer = self.code
    local pattern = "^goto " .. self.exit_label

    if string_match(code_buffer[#code_buffer], pattern) then
        code_buffer[#code_buffer] = nil
    end

    -- Unused label will break our peephole optimization, remove it if possible
    for i,line in ipairs(code_buffer) do
        if string_match(line, pattern) ~= nil then
            emit(self, "::" .. self.exit_label .. "::")
            break
        end
    end

    local func = {
        type = "function",
        name = self.function_name,
        init = self.function_init,
        locals = self.locals,
        code = self.code,
        return_reg = self.return_reg,
        exit_label = self.exit_label,
        signature = self.signature,
        parameters = self.parameters,
    }

    self.functions[self.function_name] = func
end

function Codegen.number_literal(self, x)
    if string_find(tostring(x), "[e%.]") == nil then
        return {type="int", value=x, code="", constexpr=true}
    else
        return {type="float", value=x, code="", constexpr=true}
    end
end

function Codegen.bool_literal(self, x)
    return {type="bool", value=x, code="", constexpr=true}
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

    if builtin_functions[x] ~= nil then
        local codegen = builtin_functions[x]
        return {type="builtin", value=x, codegen=codegen}
    end

    if self.functions[x] ~= nil then
        return self.functions[x]
    end

    error("undefined variable " .. x)
end

local function make_codegen(func) -- TODO: Cache Codegen
    local funcname = func.value
    local funcimpl = func.impl
    local signature = func.signature

    return function(self, params)
        local codes = {}
        local args = {}
        local all_constants = true

        for i,param in ipairs(params) do
            print(table2sexpr(param))
            table_extend(codes, param.code)
            args[i] = param.value
            if not param.constexpr then all_constants = false end
        end

        local return_type = nil
        for i=1,#signature do -- Search for matched signature
            return_type = check_signature(signature[i], params)
            if return_type ~= nil then break end
        end

        assert(return_type ~= nil, "no matched function " .. tostring(func.name) .. " declared")

        local reg = new_reg(self, funcname, return_type)
        if all_constants and funcimpl ~= nil then
            return { type="float", value=funcimpl(table.unpack(args)), code="", constexpr=true }
        end

        emit_finit(self, "local " .. reg)
        emit_to_buffer(codes, reg.."="..funcname.."("..table.concat(args, ",")..")")
        return { type="float", value=reg, code=codes }
    end
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
                func.codegen = make_codegen(func)
            end

            return func
        end
    end

    error("Not implemented")
end

function Codegen.begin_for(self, name, begin, ends, step)
    fold_code(begin)
    fold_code(ends)
    fold_code(step)
    assert(begin.type == "int" and ends.type == "int" and step.type == "int", "begin, end and step should be integer value")
    local reg = new_reg(self, name, "int")
    self.locals[name] = {type="int", value=reg, code=""}
    emit(self, begin.code)
    emit(self, ends.code)
    emit(self, step.code)
    emit(self, "for " .. reg .. "=" .. begin.value .. "," .. ends.value .. "," .. step.value .. " do")
    self.indent = self.indent + 1
end

function Codegen.end_for(self)
    self.indent = self.indent - 1
    emit(self, "end")
end

function Codegen.begin_while(self, cond)
    fold_code(cond)
    emit(self, cond.code)
    emit(self, "while " .. tostring(cond.value) .. " do")
    local stack = self.while_stack
    stack[#stack+1] = cond
    self.indent = self.indent + 1
end

function Codegen.end_while(self)
    local stack = self.while_stack
    local cond = stack[#stack]
    stack[#stack] = nil
    emit(self, cond.code)
    self.indent = self.indent - 1
    emit(self, "end")
end

function Codegen.begin_if(self, cond)
    fold_code(cond)
    emit(self, cond.code)
    emit(self, "if "..tostring(cond.value).." then")
    self.indent = self.indent + 1
    self.if_level = self.if_level + 1
end

function Codegen.begin_elseif(self, cond)
    fold_code(cond)
    if #cond.code == 0 then
        self.indent = self.indent - 1
        emit(self, "elseif "..tostring(cond.value).." then")
        self.indent = self.indent + 1
        return
    end

    self.indent = self.indent - 1
    emit(self, "else")
    self.indent = self.indent + 1
    Codegen.begin_if(self, cond)
end

function Codegen.begin_else(self)
    self.indent = self.indent - 1
    emit(self, "else")
    self.indent = self.indent + 1
end

function Codegen.end_if(self)
    for i=1,self.if_level do
        self.indent = self.indent - 1
        emit(self, "end")
    end
end

local Lexer = require("lexer")
local Lexer_new = Lexer.new
local Lexer_next = Lexer.next

function Codegen.export_code(self, function_name)
    -- Generate Wrapper Code for Functions
    local func = self.functions[function_name]
    assert(func ~= nil)
    local code = {}
    local passed_registers = {}
    code[1] = "return function("
    for i,param in ipairs(func.parameters) do
        if param.type == "float" or param.type == "int" or param.type == "bool" then
            -- Just pass then as register
            code[#code+1] = param.value
            code[#code+1] = ","
            passed_registers[param.value] = true
        else
            error("Not implemented")
        end
    end
    if code[#code] == "," then 
        code[#code] = ")\n"
    else
        code[#code+1] = ")\n"
    end

    code = {table.concat(code)} -- Assemble the first line
    local init = func.init
    local declared_locals = {}
    local referenced_locals = {}

    for _,line in ipairs(init) do
        local i, j = string_find(line, "^local ")
        local reg = string_sub(line, j+1, #line-1) -- Remove line break to get register name
        declared_locals[reg] = true
    end

    -- Scan code for reference
    for _,line in ipairs(func.code) do
        local lexer = Lexer_new(line)
        while true do
            local token = Lexer_next(lexer)
            if token == nil then break end

            if declared_locals[token] ~= nil then
                referenced_locals[token] = true
            end
        end
    end

    for _,line in ipairs(init) do
        local i, j = string_find(line, "^local ")
        local blocked = false
        if i ~= nil then
            local reg = string_sub(line, j+1, #line-1) -- Remove line break to get register name

            -- If a register are passed from outside or not referenced, remove it from declaration
            blocked = passed_registers[reg] ~= nil or referenced_locals[reg] == nil
        end

        if not blocked then code[#code+1] = line end
    end

    table_extend(code, func.code)
    if func.return_reg ~= nil then
        code[#code+1] = "return " .. func.return_reg.value .. "\n"
    end
    code[#code+1] = "end\n"

    return table_concat(self.init_code) .. table_concat(code)
end

function Codegen.export(self, function_name)
    local code = Codegen.export_code(self, function_name)
    return load(code)()
end

function Codegen.__index(t, key)
    return Codegen[key]
end

function Codegen.new()
    local self = {
        regid = 0,
        used_features = {},
        functions = {},
        globals = {},
        while_stack = {},
        init_code = {},
        if_level = 0,
        indent = 0,
        indent_char = 0,
    }

    setmetatable(self, Codegen)
    return self
end

local Parser = require("parser")
local function test_codegen()
    local code = [[
        function clamp(x :float, lower :float, upper :float) :float
            return math.max(math.min(x, upper), lower)
        end

        function smoothstep(edge0 :float, edge1 :float, x :float) :float
            local t:float = clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0)
            return t * t * (3.0 - 2.0 * t)
        end

        function test_func(x:int, y:float, z:int) : void
            local a <const> :float = 1 + 2 + 3 * 2 + 4 + math.sin(24.0)
            local b <const> :float = a + a * 3 * 5
            local c :float = math.sin(b)
            c = 114514
            local d :int = 1 + 3 * 4
            local e :float = math.min(d, d)
            
            for i=1,d do
                e = e * 2
            end

            if e > 3.4 then
                e = 1.0
            elseif e > 2.1 then
                return
            end

            e = smoothstep(e, e + 0.1, 0.5)

            while math.log(e * e) > 5.0 - 1.0 do
                e = e / 2.0
                d = int(e) + z * z
            end
        end
    ]]
    local cg = Codegen.new()
    cg.indent_char = "  "
    local parser = Parser.new(code, cg, "main")

    Parser.parse(parser)
    local ok, message = pcall(Parser.parse, parser)
    if not ok then
        print("error: " .. message)
        print("file: " .. parser.filename)
        print("line: " .. parser.line)
    else
        print(cg:export_code("smoothstep"))
        local smoothstep = cg:export("smoothstep")
        print(smoothstep(10, 20, 18))
        -- print(table.concat(cg.init_code))
        -- print(table.concat(cg.functions.test_func.init))
        -- print(table.concat(cg.functions.test_func.code))
    end
end

test_codegen()