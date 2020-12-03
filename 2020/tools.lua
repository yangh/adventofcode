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
        local lines = {}
        local part = ""

        if part_ then part = "-" .. part_ end

        local filename = "inputs/d" .. day .. part .. ".txt"
        print("Load input", filename)

        if not file_exist(filename) then return nil end

        io.input(filename)

        for l in io.lines() do
                table.insert(lines, l)
        end

        return lines
end

function T.input_lines_to_number(d, p)
        local lines = T.input(d, p)
        local numbers = {}

        if not lines then return nil end

        for _, l in ipairs(lines) do
                table.insert(numbers, tonumber(l))
        end

        return numbers
end

function T.fold_lines (lines, func)
        local count = 0
        for _, l in ipairs(lines) do
                if func(l) then
                        count = count + 1
                        --print(l)
                end
        end
        return count
end

return T