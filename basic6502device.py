class Device:
    import io
    import sys

    def __init__(self, ROM, *, input: io.BufferedReader = sys.stdin.buffer, output: io.BufferedWriter = sys.stdout.buffer):
        if not isinstance(input, self.io.BufferedReader):
            raise TypeError('A readable binary input stream is required')
        if not isinstance(output, self.io.BufferedWriter):
            raise TypeError('A writable binary output stream is required')
        ROM = list(ROM)[: 0x8000 - 0x0200]
        self.mem = (1 << 16) * [0x00]
        self.mem[0x0200: 0x0200 + len(ROM)] = ROM
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
