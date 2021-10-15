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
        self.escaped = None

    def read(self, address):
        if address == 0x00ff:
            if self.escaped != None:
                escaped = self.escaped
                self.escaped = None
                return escaped
            byte = self.input.read(1)
            if byte:
                byte = byte[0]
                if byte in (0xfe, 0xff):
                    self.escaped = byte
                    return 0xfe
                return byte
            return 0xff
        return self.mem[address]

    def write(self, address, data):
        if 0x0200 <= address < 0x8000 or 0xfffa <= address:
            return
        if address == 0x00ff:
            self.output.write(bytes((data,)))
        self.mem[address] = data
