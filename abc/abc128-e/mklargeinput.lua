local n = 2e5
local q = 2e5
math.randomseed(os.time())
io.output("large.txt")
io.write(string.format("%d %d\n", n, q))
for i = 1, n do
  local x = 10^8 - 10 * i
  local s = math.random(0, 100) + x
  local t = 10^8 - math.random(0, 100) + x
  io.write(string.format("%d %d %d\n", s, t, x))
end
for i = 1, q do
  local d = 10^9 / q * i
  io.write(string.format("%d\n", d))
end
