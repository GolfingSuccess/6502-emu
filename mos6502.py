# Warning: illegal opcodes not supported!

class MOS6502:
    '''Use __init__ to initialize a CPU.
Use NMI, RES and IRQ to send interrupt requests.
Use step to execute an instruction.
Access to other attributes is unsupported.'''
    PNames = ('C', 'Z', 'I', 'D', 'B', 'm', 'V', 'N')

    # Class properties
    def __getattr__(self, name):
        if name in self.PNames:
            return self.getFlag(name, '')
        raise AttributeError(f'Attribute {name} does not exist')

    def __setattr__(self, name, value):
        if name in self.PNames:
            mask = 1
        elif name in ('A', 'P', 'RES_A', 'RES_P', 'RES_X', 'RES_Y', 'S', 'X', 'Y'):
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
            self.setFlag(name, '', value)
        else:
            self.__dict__[name] = value

    def __init__(self, memRead, memWrite, *, A=0, X=0, Y=0, S=0xff, C=0, Z=1, B=1, V=0, N=0, m=1):
        '''Initializes a CPU connected to memory reader memRead and memory writer memWrite, and passes the keyword arguments verbatim to RES.'''
        self.MR = memRead
        self.MW = memWrite
        self.P = self.RES_P = 0b00000100
        self.RES(A=A, X=X, Y=Y, S=S, C=C, Z=Z, B=B, V=V, N=N, m=m)

    # Misc methods
    def getFlag(self, flag, prefix):
        if prefix == 'RES_':
            register = self.RES_P
        if prefix == '':
            register = self.P
        return register >> self.PNames.index(flag) & 1

    def setFlag(self, flag, prefix, value):
        if flag in ('D', 'I') and prefix == 'RES_':
            return
        mask = 1 << self.PNames.index(flag)
        value &= 1
        if prefix == 'RES_':
            self.RES_P = self.RES_P | mask if value else self.RES_P & ~mask
        if prefix == '':
            self.P = self.P | mask if value else self.P & ~mask

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

    def pushP(self, B):
        self.B = B
        self.push(self.P)

    def pullP(self):
        self.P = self.pull()
        self.B = 1
        self.m = self.getFlag('m', 'RES_')

    def readPC(self, address):
        self.PC = self.readWord(address)

    # Base I/O
    def readByte(self, address):
        if address == 'A':
            return self.A
        if isinstance(address, int):
            return self.MR(address & 0xffff) & 0xff
        if address[0] == 'I':
            return address[1] & 0xff

    def writeByte(self, address, byte):
        if address == 'A':
            self.A = byte
        else:
            self.MW(address & 0xffff, byte & 0xff)

    # Word reading
    def readWordMasked(self, address, mask):
        return self.readByte(address & mask) | self.readByte(address + 1 & mask) << 8

    def readWord(self, address):
        return self.readWordMasked(address, 0xffff)

    # Reset and interrupts

    def interrupt(self, address):
        self.pushPC()
        self.pushP(0)
        self.readPC(address)

    def NMI(self):
        '''Sends a non-maskable interrupt request.'''
        self.interrupt(0xfffa)

    def RES(self, *, A=None, X=None, Y=None, S=None, C=None, Z=None, B=None, V=None, N=None, m=None):
        '''Sends a reset signal. The keyword arguments control the initial states of various registers (m is the unused status flag).

If a keyword argument is omitted, its last used value will be used.'''
        self.A = self.RES_A = A if A != None else self.RES_A
        self.X = self.RES_X = X if X != None else self.RES_X
        self.Y = self.RES_Y = Y if Y != None else self.RES_Y
        self.S = self.RES_S = S if S != None else self.RES_S
        for i in ('C', 'Z', 'B', 'V', 'N', 'm'):
            flag = eval(i)
            if flag != None:
                self.setFlag(i, '', flag)
                self.setFlag(i, 'RES_', flag)
            else:
                self.setFlag(i, '', self.getFlag(i, 'RES_'))
        self.D = 0
        self.I = 1
        self.readPC(0xfffc)

    def IRQ(self):
        '''Sends a maskable interrupt request.'''
        if not self.I:
            self.interrupt(0xfffe)

    # Main rote
    def step(self):
        '''Executes and returns an instruction.'''
        # "ALU fragment"

        def readZPWord(address):
            return self.readWordMasked(address, 0xff)

        def setFlags(result, flags):
            if 'C' in flags:
                self.C = result >> 8
            result &= 0xff
            if 'Z' in flags:
                self.Z = not result
            if 'N' in flags:
                self.N = result >> 7

        def setVFlag(result):
            self.V = not -0x80 <= result < 0x80

        def advancePC(delta):
            self.PC += delta

        def branch(flag, address):
            if flag:
                self.PC = address

        def de2C(byte):
            return (byte & 0x7f) - ((byte & 0x80) << 1)

        def condBitNeg(byte, op):
            return {'+': byte, '-': ~byte}[op]

        def arithOp(byte, op):
            sum = de2C(self.A) + condBitNeg(de2C(byte), op) + self.C
            uSum = self.A + condBitNeg(byte, op) + self.C
            if not self.D or op == '-':
                setVFlag(sum)
                setFlags(uSum, 'CN')
            setFlags(uSum, 'Z')
            if self.D:
                AH = self.A & 0xf0
                bH = byte & 0xf0
                sign = {'+': 1, '-': -1}[op]
                dSL = (self.A & 0x0f) + condBitNeg(byte & 0x0f, op) + self.C
                if op == '+' and dSL >= 0x0a or op == '-' and dSL < 0:
                    dSL = ((dSL + sign * 0x06) & 0x0f) + sign * 0x10
                dS = AH + sign * bH + dSL
                if op == '+' and dS >= 0xa0 or op == '-' and dS < 0:
                    dS += sign * 0x60
                if op == '+':
                    setFlags(dS, 'C')
                    dSC = de2C(AH) + de2C(bH) + dSL
                    setFlags(dSC, 'N')
                    setVFlag(dSC)
                editA(dS)
            else:
                editA(uSum)

        def shift(address, op):
            original = self.readByte(address)
            if op in ('<<', '<-'):
                shifted = original << 1 | (op == '<-' and self.C)
                setFlags(shifted, 'C')
            if op in ('>>', '->'):
                shifted = original >> 1 | (op == '->' and self.C << 7)
                self.C = original
            self.writeByte(shifted, address)

        def pullPC():
            self.PC = self.pull() | self.pull() << 8

        def compare(register, byte):
            setFlags(register - byte, 'CZN')

        def editM(address, offset):
            result = self.readByte(address) + offset
            self.writeByte(address, result)
            setFlags(result, 'ZN')

        def editA(byte, op=':='):
            if op == '&':
                self.A &= byte
            if op == '^':
                self.A ^= byte
            if op == '|':
                self.A |= byte
            if op == ':=':
                self.A = byte
            setFlags(self.A, 'ZN')

        def editX(byte, op=':='):
            if op == '+':
                self.X += byte
            if op == ':=':
                self.X = byte
            setFlags(self.X, 'ZN')

        def editY(byte, op=':='):
            if op == '+':
                self.Y += byte
            if op == ':=':
                self.Y = byte
            setFlags(self.Y, 'ZN')
        # Addressing modes

        def addrMode(bytes):
            def decorate(addrMode):
                from functools import wraps
                if bytes == -1:
                    def advance(instruction):
                        instruction()
                        return []
                if bytes == 0:
                    def advance(instruction):
                        instruction(addrMode())
                        return []
                if bytes == 1:
                    def advance(instruction):
                        address = self.readByte(self.PC)
                        advancePC(1)
                        instruction(addrMode(address))
                        return [address]
                if bytes == 2:
                    def advance(instruction):
                        address = self.readWord(self.PC)
                        advancePC(2)
                        instruction(addrMode(address))
                        return [address & 0xff, address >> 8]
                return wraps(addrMode)(advance)
            return decorate

        @addrMode(2)
        def ABS(address):
            'Absolute'
            return address

        @addrMode(2)
        def AIX(address):
            'Absolute Indexed with X'
            return address + self.X

        @addrMode(2)
        def AIY(address):
            'Absolute Indexed with Y'
            return address + self.Y

        @addrMode(2)
        def ABI(address):
            'Absolute Indirect'
            return self.readWord(address)

        @addrMode(0)
        def ACC():
            'Accumulator'
            return 'A'

        @addrMode(1)
        def IMM(address):
            'Immediate Addressing'
            return ['I', address]

        @addrMode(-1)
        def ISA():
            'Implied / Stack'
            pass

        @addrMode(1)
        def PCR(address):
            'Program Counter Relative'
            return self.PC + (address + 0x80 & 0xff) - 0x80

        @addrMode(1)
        def ZPG(address):
            'Zero Page'
            return address

        @addrMode(1)
        def ZII(address):
            'Zero Page Indexed Indirect'
            return readZPWord(address + self.X)

        @addrMode(1)
        def ZIX(address):
            'Zero Page Indexed with X'
            return address + self.X & 0xff

        @addrMode(1)
        def ZIY(address):
            'Zero Page Indexed with Y'
            return address + self.Y & 0xff

        @addrMode(1)
        def ZI2(address):
            'Zero Page Indirect Indexed with Y'
            return readZPWord(address) + self.Y

        def ADC(address):
            arithOp(self.readByte(address), '+')

        def AND(address):
            editA(self.readByte(address), '&')

        def ASL(address):
            shift(address, '<<')

        def BCC(address):
            branch(not self.C, address)

        def BCS(address):
            branch(self.C, address)

        def BEQ(address):
            branch(self.Z, address)

        def BIT(byte):
            self.V = byte >> 6
            setFlags(byte, 'N')
            setFlags(self.A & byte, 'Z')

        def BMI(address):
            branch(self.N, address)

        def BNE(address):
            branch(not self.Z, address)

        def BPL(address):
            branch(not self.N, address)

        def BRK():
            advancePC(2)
            self.pushPC()
            self.pushP(1)
            self.readPC(0xfffe)

        def BVC(address):
            branch(not self.V, address)

        def BVS(address):
            branch(self.V, address)

        def CLC():
            self.C = 0

        def CLD():
            self.D = 0

        def CLI():
            self.I = 0

        def CLV():
            self.V = 0

        def CMP(address):
            compare(self.A, self.readByte(address))

        def CPX(address):
            compare(self.X, self.readByte(address))

        def CPY(address):
            compare(self.Y, self.readByte(address))

        def DEC(address):
            editM(address, -1)

        def DEX():
            editX(0xff, '+')

        def DEY():
            editY(0xff, '+')

        def EOR(address):
            editA(self.readByte(address), '^')

        def INC(address):
            editM(address, 1)

        def INX():
            editX(0x01, '+')

        def INY():
            editY(0x01, '+')

        def JMP(address):
            self.PC = address

        def JSR(address):
            advancePC(-1)
            self.pushPC()
            self.PC = address

        def LDA(address):
            editA(self.readByte(address))

        def LDX(address):
            editX(self.readByte(address))

        def LDY(address):
            editY(self.readByte(address))

        def LSR(address):
            shift(address, '>>')

        def NOP():
            pass

        def ORA(address):
            editA(self.readByte(address), '|')

        def PHA():
            self.push(self.A)

        def PHP():
            self.pushP(1)

        def PLA():
            editA(self.pull())

        def PLP():
            self.pullP()

        def ROL(address):
            shift(address, '<-')

        def ROR(address):
            shift(address, '->')

        def RTI():
            PLP()
            pullPC()

        def RTS():
            pullPC()
            advancePC(1)

        def SBC(address):
            arithOp(self.readByte(address), '-')

        def SEC():
            self.C = 1

        def SED():
            self.D = 1

        def SEI():
            self.I = 1

        def STA(address):
            self.writeByte(address, self.A)

        def STX(address):
            self.writeByte(address, self.X)

        def STY(address):
            self.writeByte(address, self.Y)

        def TAX():
            editX(self.A)

        def TAY():
            editY(self.A)

        def TSX():
            editX(self.S)

        def TXA():
            editA(self.X)

        def TXS():
            self.S = self.X

        def TYA():
            editA(self.Y)

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

        byte = self.readByte(self.PC)
        try:
            op = ops[byte]
        except KeyError:
            raise RuntimeError(f'Opcode {byte:02X} is illegal!')
        advancePC(1)
        return [byte, *op[0](op[1])]
