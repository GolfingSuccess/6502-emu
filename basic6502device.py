class Device:
    import io
    import sys

    def __init__(self, ro, *, input: io.BytesIO = sys.stdin.buffer, output: io.BytesIO = sys.stdout.buffer):
        ro = list(ro)[: 0x8000 - 0x0200]
        self.mem = (1 << 16) * [0x00]
        self.mem[0x0200: 0x0200 + len(ro)] = ro
        self.mem[0xfffa:] = [0x00, 0x70,
                             0x00, 0x02,
                             0x00, 0x60]
        self.input = input
        self.output = output

    def read(self, address):
        if address == 0x00ff:
            byte = self.input.read(1)
            self.mem[address] = byte[0] if byte else 0xff
        return self.mem[address]

    def write(self, address, data):
        if 0x0200 <= address < 0x8000 or 0xfffa <= address:
            return
        if address == 0x00ff:
            self.output.write(bytes((data,)))
        self.mem[address] = data
