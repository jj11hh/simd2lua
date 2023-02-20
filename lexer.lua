-- Lexer for simd2lua

Lexer = {}

function Lexer.new(input)
    local self = {
        input = input,
        position = 1,
        current = nil,
        backtrace = {},
        line = 1,
        line_pos = 1,
    }

    Lexer.next(self)

    return self
end

local function skip_blanks(self)
    local pos = self.position
    local line = self.line
    local line_pos = self.line_pos
    local input = self.input
    local in_comment = false

    while pos <= #input do
        local char = string.sub(input, pos, pos)
        local is_blank = char == " " or char == "\t" or char == "\r" or char == "\n"

        if not in_comment and not is_blank then break end

        if char == "-" then -- Maybe comment
            if pos+1 <= #input and string.sub(input, pos+1, pos+1) == "-" then
                in_comment = true
            end
        end

        if char == "\n" then
            line = line + 1
            line_pos = 0
            in_comment = false
        else
            line_pos = line_pos + 1
        end
        pos = pos + 1
    end

    self.position = pos
    self.line = line
    self.line_pos = line_pos
end

local operators = {
    ["+"] = false,
    ["-"] = false,
    ["*"] = false,
    ["/"] = {"//"},
    ["^"] = false,
    ["%"] = false,
    ["="] = {"=="},
    ["~"] = {"~="},
    ["<"] = {"<=", "<<"},
    [">"] = {">=", ">>"},
    ["#"] = false,
    ["&"] = false,
    ["|"] = false,
    ["."] = {".."},
}

local constants = {
    ["true"] = true,
    ["false"] = false,
}

local string_find = string.find
local string_match = string.match
local string_sub = string.sub

-- Copy from https://github.com/GavinHigham/lpil53/blob/master/lexer.lua, with some modification
local function lex_numeric(self)
    local input = self.input
    local init = self.position

	local i, j, token = string_find(input, '^(%d*%.?%d+)', init) --Check for numeric constants
	local exponent = '^([eE][+-]?%d+)' --Pattern for decimal exponent
	if not i then
		i, j, token = string_find(input, '^(0[xX]%x*%.?%x+)', init) --Check for hexidecimal constants
		if i then
			exponent = '^([pP][+-]?%x+)' --Pattern for binary exponent
		else
			printParserDebug(input, init, lines, "Error: Didn't expect this here.", true)
		end
    else
		local i2, j2, token2 = string_find(input, exponent, j+1) --Check for trailing exponent
		if i2 then
			j, token = j2, token..token2
		end
	end
	return i, j, token
end

local function lexer_lex(self)
    local chars = {}
    skip_blanks(self)

    local input = self.input
    local pos = self.position

    local char = string_sub(input, pos, pos)
    local token = char

    if string_match(char, "[a-zA-Z_]") then -- Most time here
        local i, j = string_find(input, "^[0-9a-zA-Z_]+")
        token = string_sub(input, i, j)
        pos = j+1
        local constant = constants[token]
        if constant ~= nil then token = constant end
    else 
        local op = operators[char]
        if op ~= nil then -- Operators
            if op == false then -- Single char op
                token = char
            else -- Double char op
                token = string_sub(input, pos, pos+1) -- No need for boundary check
                for i=1,#op do
                    if token == op[i] then
                        token = op[i]
                        pos = pos + #token
                        break
                    end
                end
            end
        else
            if string_match(char, "[0-9.]")
        end
    end

    self.current = token
    self.position = pos
    return token
end

function Lexer.next(self)
    token, new_pos, line, line_pos = lexer_lex(self)
    self.current = token
    self.position = new_pos
    self.line = line
    self.line_pos = line_pos

    return token
end
