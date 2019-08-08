math.randomseed(os.time())

local a = string.byte "a"
local s, t = {}, {}
for i = 1, 100 do
  s[i] = string.char(a + math.floor(math.random() * 26))
end
for i = 1, 3000 do
  t[i] = string.char(a + math.floor(math.random() * 26))
end
local f = assert(io.open("testinput.txt", "w"))
f:write(table.concat(s), "\n")
f:write(table.concat(t), "\n")
f:close()

