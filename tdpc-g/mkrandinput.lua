math.randomseed(os.time())

local a = string.byte "a"
local t = {}
for i = 1, 10^6 do
  t[i] = string.char(a + math.floor(math.random() * 26))
end
local f = assert(io.open("testinput.txt", "w"))
f:write(table.concat(t), "\n")
f:write("1000000000000000000\n")
f:close()

