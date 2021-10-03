# Warning: illegal opcodes not supported!

class MOS6502:
    PNames = ('C', 'Z', 'I', 'D', 'B', 'm', 'V', 'N')

    # Class properties
    def __getattr__(self, name):
        if name not in self.PNames:
            raise AttributeError(f"Attribute {name} does not exist")
        return self.P >> self.PNames.index(name) & 1

    def __setattr__(self, name, value):
        if name in ('B', 'C', 'D', 'I', 'N', 'RES_B', 'RES_C', 'RES_MB', 'RES_N', 'RES_V', 'RES_Z', 'V', 'Z', 'm'):
            mask = 1
        elif name in ('A', 'P', 'RES_A', 'RES_X', 'RES_Y', 'S', 'X', 'Y'):
            mask = 8
        elif name == 'PC':
            mask = 16
        else:
            if name in ('MR', 'MW'):
                if name not in self.__dict__:
                    self.__dict__[name] = value
            return
        value &= (1 << mask) - 1
        if name in self.PNames:
            mask = 1 << self.PNames.index(name)
            self.P = self.P | mask if value else self.P & ~mask
        else:
            self.__dict__[name] = value & (1 << mask) - 1

    def __init__(self, memRead, memWrite, *, A=0, X=0, Y=0, C=0, Z=1, B=1, V=0, N=0, m=1):
        self.MR = memRead
        self.MW = memWrite
        self.P = 0
        self.RES(A=A, X=X, Y=Y, C=C, Z=Z, B=B, V=V, N=N, m=m)

    # Main rote
    def step(self):
        byte = self.readByte(self.PC)
        try:
            op = self.ops[byte]
        except KeyError:
            raise RuntimeError(f'Opcode {byte:02X} is illegal!')
        self.advancePC(1)
        return [byte, *op[0](self, op[1])]

    # Base I/O
    def readByte(self, address):
        if address == 'A':
            return self.A
        return self.MR(address & 0xffff) & 0xff

    def writeByte(self, address, byte):
        if address == 'A':
            self.editA(byte)
        else:
            self.MW(address & 0xffff, byte & 0xff)

    # Word reading
    def readWordMasked(self, address, mask):
        return self.readByte(address & mask) | self.readByte(address + 1 & mask) << 8

    def readWord(self, address):
        return self.readWordMasked(address, 0xffff)

    def readZPWord(self, address):
        return self.readWordMasked(address, 0xff)

    # Reset and interrupts
    def readPC(self, address):
        self.JMP(self.readWord(address))

    def interrupt(self, address):
        self.pushPC()
        self.pushP(0)
        self.readPC(address)

    def NMI(self):
        self.interrupt(0xfffa)

    def RES(self, *, A=None, X=None, Y=None, C=None, Z=None, B=None, V=None, N=None, m=None):
        self.A = self.RES_A = A if A != None else self.RES_A
        self.X = self.RES_X = X if X != None else self.RES_X
        self.Y = self.RES_Y = Y if Y != None else self.RES_Y
        self.C = self.RES_C = C if C != None else self.RES_C
        self.Z = self.RES_Z = Z if Z != None else self.RES_Z
        self.B = self.RES_B = B if B != None else self.RES_B
        self.V = self.RES_V = V if V != None else self.RES_V
        self.N = self.RES_N = N if N != None else self.RES_N
        self.m = self.RES_m = m if m != None else self.RES_m
        self.S = 0xff
        self.CLD()
        self.SEI()
        self.readPC(0xfffc)

    def IRQ(self):
        if not self.I:
            self.interrupt(0xfffe)

    # "ALU fragment"
    def setFlags(self, result, flags):
        if 'C' in flags:
            self.C = result >> 8
        result &= 0xff
        if 'Z' in flags:
            self.Z = not result
        if 'N' in flags:
            self.N = result >> 7

    def setVFlag(self, result):
        self.V = not -0x80 <= result < 0x80

    def advancePC(self, delta):
        self.JMP(self.PC + delta)

    def de2C(self, byte):
        return (byte & 0x7f) - ((byte & 0x80) << 1)
    
    def condBitNeg(self, byte, op):
        return {'+': byte, '-': ~byte}[op]

    def arithOp(self, byte, op):
        sum = self.de2C(self.A) + self.condBitNeg(self.de2C(byte), op) + self.C
        uSum = self.A + self.condBitNeg(byte, op) + self.C
        if not self.D or op == '-':
            self.setVFlag(sum)
            self.setFlags(uSum, 'CN')
        self.setFlags(uSum, 'Z')
        if self.D:
            AH = self.A & 0xf0
            bH = byte & 0xf0
            sign = {'+': 1, '-': -1}[op]
            dSL = (self.A & 0x0f) + self.condBitNeg(byte & 0x0f, op) + self.C
            if op == '+' and dSL >= 0x0a or op == '-' and dSL < 0:
                dSL = ((dSL + sign * 0x06) & 0x0f) + sign * 0x10
            dS = AH + sign * bH + dSL
            if op == '+' and dS >= 0xa0 or op == '-' and dS < 0:
                dS += sign * 0x60
            if op == '+':
                self.setFlags(dS, 'C')
                dSC = self.de2C(AH) + self.de2C(bH) + dSL
                self.setFlags(dSC, 'N')
                self.setVFlag(dSC)
            self.editA(dS)
        else:
            self.editA(uSum)

    def shift(self, address, op):
        original = self.readByte(address)
        if op in ('<<', '<-'):
            shifted = original << 1 | (op == '<-' and self.C)
            self.setFlags(shifted, 'C')
        if op in ('>>', '->'):
            shifted = original >> 1 | (op == '->' and self.C << 7)
            self.C = original
        self.writeByte(shifted, address)

    def push(self, byte):
        self.writeByte(0x0100 | self.S, byte)
        self.S -= 1

    def pull(self):
        self.S += 1
        byte = self.readByte(0x0100 | self.S)
        return byte

    def pushPC(self):
        self.push(self.PC >> 8)
        self.push(self.PC)

    def pullPC(self):
        self.PC = self.pull() | self.pull() << 8

    def pushP(self, B):
        self.B = B
        self.push(self.C << 0 |
                  self.Z << 1 |
                  self.I << 2 |
                  self.D << 3 |
                  (B & 1) << 4 |
                  self.m << 5 |
                  self.V << 6 |
                  self.N << 7)

    def compare(self, register, byte):
        self.setFlags(register - byte, 'CZN')

    def editM(self, address, offset):
        result = self.readByte(address) + offset
        self.writeByte(address, result)
        self.setFlags(result, 'ZN')

    def editA(self, byte, op=':='):
        if op == '&':
            self.A &= byte
        if op == '^':
            self.A ^= byte
        if op == '|':
            self.A |= byte
        if op == ':=':
            self.A = byte
        self.setFlags(self.A, 'ZN')

    def editX(self, byte, op=':='):
        if op == '+':
            self.X += byte
        if op == ':=':
            self.X = byte
        self.setFlags(self.X, 'ZN')

    def editY(self, byte, op=':='):
        if op == '+':
            self.Y += byte
        if op == ':=':
            self.Y = byte
        self.setFlags(self.Y, 'ZN')

    # Addressing modes
    def addrMode(bytes):
        def decorate(addrMode):
            from functools import wraps
            decorator = wraps(addrMode)
            if bytes == -1:
                @decorator
                def advance(self, instruction):
                    instruction(self)
                    return []
            if bytes == 0:
                @decorator
                def advance(self, instruction):
                    instruction(self, addrMode(self))
                    return []
            if bytes == 1:
                @decorator
                def advance(self, instruction):
                    address = self.readByte(self.PC)
                    self.advancePC(1)
                    instruction(self, addrMode(self, address))
                    return [address]
            if bytes == 2:
                @decorator
                def advance(self, instruction):
                    address = self.readWord(self.PC)
                    self.advancePC(2)
                    instruction(self, addrMode(self, address))
                    return [address & 0xff, address >> 8]
            return advance
        return decorate

    @addrMode(2)
    def ABS(self, address):
        'Absolute'
        return address

    @addrMode(2)
    def AII(self, address):
        'Absolute Indexed Indirect'
        return self.readWord(address + self.X)

    @addrMode(2)
    def AIX(self, address):
        'Absolute Indexed with X'
        return address + self.X

    @addrMode(2)
    def AIY(self, address):
        'Absolute Indexed with Y'
        return address + self.Y

    @addrMode(2)
    def ABI(self, address):
        'Absolute Indirect'
        return self.readWord(address)

    @addrMode(0)
    def ACC(self):
        'Accumulator'
        return 'A'

    @addrMode(1)
    def IMM(self, _):
        'Immediate Addressing'
        return self.PC - 1

    @addrMode(-1)
    def ISA(self):
        'Implied / Stack'
        pass

    @addrMode(1)
    def PCR(self, address):
        'Program Counter Relative'
        return self.PC + (address + 0x80 & 0xff) - 0x80

    @addrMode(1)
    def ZPG(self, address):
        'Zero Page'
        return address

    @addrMode(1)
    def ZII(self, address):
        'Zero Page Indexed Indirect'
        return self.readZPWord(address + self.X)

    @addrMode(1)
    def ZIX(self, address):
        'Zero Page Indexed with X'
        return address + self.X & 0xff

    @addrMode(1)
    def ZIY(self, address):
        'Zero Page Indexed with Y'
        return address + self.Y & 0xff

    @addrMode(1)
    def ZI2(self, address):
        'Zero Page Indirect Indexed with Y'
        return self.readZPWord(address) + self.Y

    def ADC(self, address):
        self.arithOp(self.readByte(address), '+')

    def AND(self, address):
        self.editA(self.readByte(address), '&')

    def ASL(self, address):
        self.shift(address, '<<')

    def BCC(self, address):
        if not self.C:
            self.JMP(address)

    def BCS(self, address):
        if self.C:
            self.JMP(address)

    def BEQ(self, address):
        if self.Z:
            self.JMP(address)

    def BIT(self, byte):
        self.V = byte >> 6
        self.setFlags(byte, 'N')
        self.setFlags(self.A & byte, 'Z')

    def BMI(self, address):
        if self.N:
            self.JMP(address)

    def BNE(self, address):
        if not self.Z:
            self.JMP(address)

    def BPL(self, address):
        if not self.N:
            self.JMP(address)

    def BRK(self):
        self.advancePC(2)
        self.pushPC()
        self.PHP()
        self.readPC(0xfffe)

    def BVC(self, address):
        if not self.V:
            self.JMP(address)

    def BVS(self, address):
        if self.V:
            self.JMP(address)

    def CLC(self):
        self.C = 0

    def CLD(self):
        self.D = 0

    def CLI(self):
        self.I = 0

    def CLV(self):
        self.V = 0

    def CMP(self, address):
        self.compare(self.A, self.readByte(address))

    def CPX(self, address):
        self.compare(self.X, self.readByte(address))

    def CPY(self, address):
        self.compare(self.Y, self.readByte(address))

    def DEC(self, address):
        self.editM(address, -1)

    def DEX(self):
        self.editX(0xff, '+')

    def DEY(self):
        self.editY(0xff, '+')

    def EOR(self, address):
        self.editA(self.readByte(address), '^')

    def INC(self, address):
        self.editM(address, 1)

    def INX(self):
        self.editX(0x01, '+')

    def INY(self):
        self.editY(0x01, '+')

    def JMP(self, address):
        self.PC = address

    def JSR(self, address):
        self.advancePC(-1)
        self.pushPC()
        self.JMP(address)

    def LDA(self, address):
        self.editA(self.readByte(address))

    def LDX(self, address):
        self.editX(self.readByte(address))

    def LDY(self, address):
        self.editY(self.readByte(address))

    def LSR(self, address):
        self.shift(address, '>>')

    def NOP(self):
        pass

    def ORA(self, address):
        self.editA(self.readByte(address), '|')

    def PHA(self):
        self.push(self.A)

    def PHP(self):
        self.pushP(1)

    def PLA(self):
        self.editA(self.pull())

    def PLP(self):
        sr = self.pull()
        self.C = sr >> 0
        self.Z = sr >> 1
        self.I = sr >> 2
        self.D = sr >> 3
        self.B = 1
        self.V = sr >> 6
        self.setFlags(sr, 'N')

    def ROL(self, address):
        self.shift(address, '<-')

    def ROR(self, address):
        self.shift(address, '->')

    def RTI(self):
        self.PLP()
        self.pullPC()

    def RTS(self):
        self.pullPC()
        self.advancePC(1)

    def SBC(self, address):
        self.arithOp(self.readByte(address), '-')

    def SEC(self):
        self.C = 1

    def SED(self):
        self.D = 1

    def SEI(self):
        self.I = 1

    def STA(self, address):
        self.writeByte(address, self.A)

    def STX(self, address):
        self.writeByte(address, self.X)

    def STY(self, address):
        self.writeByte(address, self.Y)

    def TAX(self):
        self.editX(self.A)

    def TAY(self):
        self.editY(self.A)

    def TSX(self):
        self.editX(self.S)

    def TXA(self):
        self.editA(self.X)

    def TXS(self):
        self.S = self.X

    def TYA(self):
        self.editA(self.Y)

    # Opcodes
    ops = {
        0x00: (ISA, BRK),
        0x01: (ZII, ORA),
        0x05: (ZPG, ORA),
        0x06: (ZPG, ASL),
        0x08: (ISA, PHP),
        0x09: (IMM, ORA),
        0x0a: (ACC, ASL),
        0x0d: (ABS, ORA),
        0x0e: (ABS, ASL),
        0x10: (PCR, BPL),
        0x11: (ZI2, ORA),
        0x15: (ZIX, ORA),
        0x16: (ZIX, ASL),
        0x18: (ISA, CLC),
        0x19: (AIY, ORA),
        0x1d: (AIX, ORA),
        0x1e: (AIX, ASL),
        0x20: (ABS, JSR),
        0x21: (ZII, AND),
        0x24: (ZPG, BIT),
        0x25: (ZPG, AND),
        0x26: (ZPG, ROL),
        0x28: (ISA, PLP),
        0x29: (IMM, AND),
        0x2a: (ACC, ROL),
        0x2c: (ABS, BIT),
        0x2d: (ABS, AND),
        0x2e: (ABS, ROL),
        0x30: (PCR, BMI),
        0x31: (ZI2, AND),
        0x35: (ZIX, AND),
        0x36: (ZIX, ROL),
        0x38: (ISA, SEC),
        0x39: (AIY, AND),
        0x3d: (AIX, AND),
        0x3e: (AIX, ROL),
        0x40: (ISA, RTI),
        0x41: (ZII, EOR),
        0x45: (ZPG, EOR),
        0x46: (ZPG, LSR),
        0x48: (ISA, PHA),
        0x49: (IMM, EOR),
        0x4a: (ACC, LSR),
        0x4c: (ABS, JMP),
        0x4d: (ABS, EOR),
        0x4e: (ABS, LSR),
        0x50: (PCR, BVC),
        0x51: (ZI2, EOR),
        0x55: (ZIX, EOR),
        0x56: (ZIX, LSR),
        0x58: (ISA, CLI),
        0x59: (AIY, EOR),
        0x5d: (AIX, EOR),
        0x5e: (AIX, LSR),
        0x60: (ISA, RTS),
        0x61: (ZII, ADC),
        0x65: (ZPG, ADC),
        0x66: (ZPG, ROR),
        0x68: (ISA, PLA),
        0x69: (IMM, ADC),
        0x6a: (ACC, ROR),
        0x6c: (ABI, JMP),
        0x6d: (ABS, ADC),
        0x6e: (ABS, ROR),
        0x70: (PCR, BVS),
        0x71: (ZI2, ADC),
        0x75: (ZIX, ADC),
        0x76: (ZIX, ROR),
        0x78: (ISA, SEI),
        0x79: (AIY, ADC),
        0x7d: (AIX, ADC),
        0x7e: (AIX, ROR),
        0x81: (ZII, STA),
        0x84: (ZPG, STY),
        0x85: (ZPG, STA),
        0x86: (ZPG, STX),
        0x88: (ISA, DEY),
        0x89: (IMM, BIT),
        0x8a: (ISA, TXA),
        0x8c: (ABS, STY),
        0x8d: (ABS, STA),
        0x8e: (ABS, STX),
        0x90: (PCR, BCC),
        0x91: (ZI2, STA),
        0x94: (ZIX, STY),
        0x95: (ZIX, STA),
        0x96: (ZIX, STX),
        0x98: (ISA, TYA),
        0x99: (AIY, STA),
        0x9a: (ISA, TXS),
        0x9d: (AIX, STA),
        0xa0: (IMM, LDY),
        0xa1: (ZII, LDA),
        0xa2: (IMM, LDX),
        0xa4: (ZPG, LDY),
        0xa5: (ZPG, LDA),
        0xa6: (ZPG, LDX),
        0xa8: (ISA, TAY),
        0xa9: (IMM, LDA),
        0xaa: (ISA, TAX),
        0xac: (ABS, LDY),
        0xad: (ABS, LDA),
        0xae: (ABS, LDX),
        0xb0: (PCR, BCS),
        0xb1: (ZI2, LDA),
        0xb4: (ZIX, LDY),
        0xb5: (ZIX, LDA),
        0xb6: (ZIY, LDX),
        0xb8: (ISA, CLV),
        0xb9: (AIY, LDA),
        0xba: (ISA, TSX),
        0xbc: (AIX, LDY),
        0xbd: (AIX, LDA),
        0xbe: (AIY, LDX),
        0xc0: (IMM, CPY),
        0xc1: (ZII, CMP),
        0xc4: (ZPG, CPY),
        0xc5: (ZPG, CMP),
        0xc6: (ZPG, DEC),
        0xc8: (ISA, INY),
        0xc9: (IMM, CMP),
        0xca: (ISA, DEX),
        0xcc: (ABS, CPY),
        0xcd: (ABS, CMP),
        0xce: (ABS, DEC),
        0xd0: (PCR, BNE),
        0xd1: (ZI2, CMP),
        0xd5: (ZIX, CMP),
        0xd6: (ZIX, DEC),
        0xd8: (ISA, CLD),
        0xd9: (AIY, CMP),
        0xdd: (AIX, CMP),
        0xde: (AIX, DEC),
        0xe0: (IMM, CPX),
        0xe1: (ZII, SBC),
        0xe4: (ZPG, CPX),
        0xe5: (ZPG, SBC),
        0xe6: (ZPG, INC),
        0xe8: (ISA, INX),
        0xe9: (IMM, SBC),
        0xea: (ISA, NOP),
        0xec: (ABS, CPX),
        0xed: (ABS, SBC),
        0xee: (ABS, INC),
        0xf0: (PCR, BEQ),
        0xf1: (ZI2, SBC),
        0xf5: (ZIX, SBC),
        0xf6: (ZIX, INC),
        0xf8: (ISA, SED),
        0xf9: (AIY, SBC),
        0xfd: (AIX, SBC),
        0xfe: (AIX, INC)
    }
