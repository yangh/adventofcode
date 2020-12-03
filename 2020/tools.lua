local T = {}

local function file_exist(file)
	local f = io.open(file)
	if f then
		io.close(f)
		return true
	else
		return false
	end
end

function T.input(day, part_)
        local part = ""

        if part_ then part = "-" .. part_ end

        local filename = "inputs/d" .. day .. part .. ".txt"
        print("Load input", filename)

        if not file_exist(filename) then return nil end

        local f = io.open(filename)
        io.input(f)

        return io.lines()
end

function T.input_lines_to_number(d, p)
        local lines = T.input(d, p)
        local numbers = {}

        if not lines then return nil end

        for l in lines do
                table.insert(numbers, tonumber(l))
        end

        return numbers
end

return T