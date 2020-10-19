using System;


namespace NESEmulator
{
    struct Instruction
    {
        public string mnemonic;
        public Func<byte> addr;
        public Func<byte> opcode;
        public byte cycles;

        public Instruction(string mnemonic, Func<byte> addr, Func<byte> opcode, byte cycles)
        {
            this.mnemonic = mnemonic;
            this.addr = addr;
            this.opcode = opcode;
            this.cycles = cycles;
        }
    }

    class Rain6502
    {
        private Bus bus;

        private byte a = 0x00; // Accumulator Register
        private byte x = 0x00; // X Register
        private byte y = 0x00; // Y Register
        private byte sp = 0x00; // Stack Pointer
        private byte pc = 0x00; // Program Counter
        private byte status = 0x00; // Status Register

        public ushort addr_abs = 0x0000;
        public ushort addr_rel = 0x0000;
        public byte fetched = 0x00;
        public byte opcode = 0x00;
        public byte cycles = 0x00;

        public Instruction[] Instructions;

        public delegate byte Hello();
        public delegate byte Opcode();

        public enum FLAGS
        {
            C = (1 << 0), // Carry Bit
            Z = (1 << 1), // Zero
            I = (1 << 2), // Disable Interrupts
            D = (1 << 3), // Decimal Mode
            B = (1 << 4), // Break
            U = (1 << 5), // Unused
            V = (1 << 6), // Overflow
            N = (1 << 7)  // Negative
        }

        public Rain6502()
        {
            this.Instructions = new Instruction[256]
               {
                     new Instruction( "BRK", BRK, IMM, 7 ) , new Instruction( "ORA", ORA, IZX, 6 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 8 ) , new Instruction( "???", NOP, IMP, 3 ) , new Instruction( "ORA", ORA, ZP0, 3 ),  new Instruction( "ASL", ASL, ZP0, 5 ) , new Instruction( "???", XXX, IMP, 5 ) , new Instruction( "PHP", PHP, IMP, 3 ) , new Instruction( "ORA", ORA, IMM, 2 ) , new Instruction( "ASL", ASL, IMP, 2 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "ORA", ORA, ABS, 4 ) , new Instruction( "ASL", ASL, ABS, 6 ) , new Instruction( "???", XXX, IMP, 6 ),
                     new Instruction( "BPL", BPL, REL, 2 ) , new Instruction( "ORA", ORA, IZY, 5 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 8 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "ORA", ORA, ZPX, 4 ) , new Instruction( "ASL", ASL, ZPX, 6 ) , new Instruction( "???", XXX, IMP, 6 ) , new Instruction( "CLC", CLC, IMP, 2 ) , new Instruction( "ORA", ORA, ABY, 4 ) , new Instruction( "???", NOP, IMP, 2 ) , new Instruction( "???", XXX, IMP, 7 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "ORA", ORA, ABX, 4 ) , new Instruction( "ASL", ASL, ABX, 7 ) , new Instruction( "???", XXX, IMP, 7 ),
                     new Instruction( "JSR", JSR, ABS, 6 ) , new Instruction( "AND", AND, IZX, 6 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 8 ) , new Instruction( "BIT", BIT, ZP0, 3 ) , new Instruction( "AND", AND, ZP0, 3 ) , new Instruction( "ROL", ROL, ZP0, 5 ) , new Instruction( "???", XXX, IMP, 5 ) , new Instruction( "PLP", PLP, IMP, 4 ) , new Instruction( "AND", AND, IMM, 2 ) , new Instruction( "ROL", ROL, IMP, 2 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "BIT", BIT, ABS, 4 ) , new Instruction( "AND", AND, ABS, 4 ) , new Instruction( "ROL", ROL, ABS, 6 ) , new Instruction( "???", XXX, IMP, 6 ),
                     new Instruction( "BMI", BMI, REL, 2 ) , new Instruction( "AND", AND, IZY, 5 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 8 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "AND", AND, ZPX, 4 ) , new Instruction( "ROL", ROL, ZPX, 6 ) , new Instruction( "???", XXX, IMP, 6 ) , new Instruction( "SEC", SEC, IMP, 2 ) , new Instruction( "AND", AND, ABY, 4 ) , new Instruction( "???", NOP, IMP, 2 ) , new Instruction( "???", XXX, IMP, 7 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "AND", AND, ABX, 4 ) , new Instruction( "ROL", ROL, ABX, 7 ) , new Instruction( "???", XXX, IMP, 7 ),
                     new Instruction( "RTI", RTI, IMP, 6 ) , new Instruction( "EOR", EOR, IZX, 6 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 8 ) , new Instruction( "???", NOP, IMP, 3 ) , new Instruction( "EOR", EOR, ZP0, 3 ) , new Instruction( "LSR", LSR, ZP0, 5 ) , new Instruction( "???", XXX, IMP, 5 ) , new Instruction( "PHA", PHA, IMP, 3 ) , new Instruction( "EOR", EOR, IMM, 2 ) , new Instruction( "LSR", LSR, IMP, 2 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "JMP", JMP, ABS, 3 ) , new Instruction( "EOR", EOR, ABS, 4 ) , new Instruction( "LSR", LSR, ABS, 6 ) , new Instruction( "???", XXX, IMP, 6 ),
                     new Instruction( "BVC", BVC, REL, 2 ) , new Instruction( "EOR", EOR, IZY, 5 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 8 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "EOR", EOR, ZPX, 4 ) , new Instruction( "LSR", LSR, ZPX, 6 ) , new Instruction( "???", XXX, IMP, 6 ) , new Instruction( "CLI", CLI, IMP, 2 ) , new Instruction( "EOR", EOR, ABY, 4 ) , new Instruction( "???", NOP, IMP, 2 ) , new Instruction( "???", XXX, IMP, 7 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "EOR", EOR, ABX, 4 ) , new Instruction( "LSR", LSR, ABX, 7 ) , new Instruction( "???", XXX, IMP, 7 ),
                     new Instruction( "RTS", RTS, IMP, 6 ) , new Instruction( "ADC", ADC, IZX, 6 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 8 ) , new Instruction( "???", NOP, IMP, 3 ) , new Instruction( "ADC", ADC, ZP0, 3 ) , new Instruction( "ROR", ROR, ZP0, 5 ) , new Instruction( "???", XXX, IMP, 5 ) , new Instruction( "PLA", PLA, IMP, 4 ) , new Instruction( "ADC", ADC, IMM, 2 ) , new Instruction( "ROR", ROR, IMP, 2 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "JMP", JMP, IND, 5 ) , new Instruction( "ADC", ADC, ABS, 4 ) , new Instruction( "ROR", ROR, ABS, 6 ) , new Instruction( "???", XXX, IMP, 6 ),
                     new Instruction( "BVS", BVS, REL, 2 ) , new Instruction( "ADC", ADC, IZY, 5 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 8 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "ADC", ADC, ZPX, 4 ) , new Instruction( "ROR", ROR, ZPX, 6 ) , new Instruction( "???", XXX, IMP, 6 ) , new Instruction( "SEI", SEI, IMP, 2 ) , new Instruction( "ADC", ADC, ABY, 4 ) , new Instruction( "???", NOP, IMP, 2 ) , new Instruction( "???", XXX, IMP, 7 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "ADC", ADC, ABX, 4 ) , new Instruction( "ROR", ROR, ABX, 7 ) , new Instruction( "???", XXX, IMP, 7 ),
                     new Instruction( "???", NOP, IMP, 2 ) , new Instruction( "STA", STA, IZX, 6 ) , new Instruction( "???", NOP, IMP, 2 ) , new Instruction( "???", XXX, IMP, 6 ) , new Instruction( "STY", STY, ZP0, 3 ) , new Instruction( "STA", STA, ZP0, 3 ) , new Instruction( "STX", STX, ZP0, 3 ) , new Instruction( "???", XXX, IMP, 3 ) , new Instruction( "DEY", DEY, IMP, 2 ) , new Instruction( "???", NOP, IMP, 2 ) , new Instruction( "TXA", TXA, IMP, 2 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "STY", STY, ABS, 4 ) , new Instruction( "STA", STA, ABS, 4 ) , new Instruction( "STX", STX, ABS, 4 ) , new Instruction( "???", XXX, IMP, 4 ),
                     new Instruction( "BCC", BCC, REL, 2 ) , new Instruction( "STA", STA, IZY, 6 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 6 ) , new Instruction( "STY", STY, ZPX, 4 ) , new Instruction( "STA", STA, ZPX, 4 ) , new Instruction( "STX", STX, ZPY, 4 ) , new Instruction( "???", XXX, IMP, 4 ) , new Instruction( "TYA", TYA, IMP, 2 ) , new Instruction( "STA", STA, ABY, 5 ) , new Instruction( "TXS", TXS, IMP, 2 ) , new Instruction( "???", XXX, IMP, 5 ) , new Instruction( "???", NOP, IMP, 5 ) , new Instruction( "STA", STA, ABX, 5 ) , new Instruction( "???", XXX, IMP, 5 ) , new Instruction( "???", XXX, IMP, 5 ),
                     new Instruction( "LDY", LDY, IMM, 2 ) , new Instruction( "LDA", LDA, IZX, 6 ) , new Instruction( "LDX", LDX, IMM, 2 ) , new Instruction( "???", XXX, IMP, 6 ) , new Instruction( "LDY", LDY, ZP0, 3 ) , new Instruction( "LDA", LDA, ZP0, 3 ) , new Instruction( "LDX", LDX, ZP0, 3 ) , new Instruction( "???", XXX, IMP, 3 ) , new Instruction( "TAY", TAY, IMP, 2 ) , new Instruction( "LDA", LDA, IMM, 2 ) , new Instruction( "TAX", TAX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "LDY", LDY, ABS, 4 ) , new Instruction( "LDA", LDA, ABS, 4 ) , new Instruction( "LDX", LDX, ABS, 4 ) , new Instruction( "???", XXX, IMP, 4 ),
                     new Instruction( "BCS", BCS, REL, 2 ) , new Instruction( "LDA", LDA, IZY, 5 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 5 ) , new Instruction( "LDY", LDY, ZPX, 4 ) , new Instruction( "LDA", LDA, ZPX, 4 ) , new Instruction( "LDX", LDX, ZPY, 4 ) , new Instruction( "???", XXX, IMP, 4 ) , new Instruction( "CLV", CLV, IMP, 2 ) , new Instruction( "LDA", LDA, ABY, 4 ) , new Instruction( "TSX", TSX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 4 ) , new Instruction( "LDY", LDY, ABX, 4 ) , new Instruction( "LDA", LDA, ABX, 4 ) , new Instruction( "LDX", LDX, ABY, 4 ) , new Instruction( "???", XXX, IMP, 4 ),
                     new Instruction( "CPY", CPY, IMM, 2 ) , new Instruction( "CMP", CMP, IZX, 6 ) , new Instruction( "???", NOP, IMP, 2 ) , new Instruction( "???", XXX, IMP, 8 ) , new Instruction( "CPY", CPY, ZP0, 3 ) , new Instruction( "CMP", CMP, ZP0, 3 ) , new Instruction( "DEC", DEC, ZP0, 5 ) , new Instruction( "???", XXX, IMP, 5 ) , new Instruction( "INY", INY, IMP, 2 ) , new Instruction( "CMP", CMP, IMM, 2 ) , new Instruction( "DEX", DEX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "CPY", CPY, ABS, 4 ) , new Instruction( "CMP", CMP, ABS, 4 ) , new Instruction( "DEC", DEC, ABS, 6 ) , new Instruction( "???", XXX, IMP, 6 ),
                     new Instruction( "BNE", BNE, REL, 2 ) , new Instruction( "CMP", CMP, IZY, 5 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 8 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "CMP", CMP, ZPX, 4 ) , new Instruction( "DEC", DEC, ZPX, 6 ) , new Instruction( "???", XXX, IMP, 6 ) , new Instruction( "CLD", CLD, IMP, 2 ) , new Instruction( "CMP", CMP, ABY, 4 ) , new Instruction( "NOP", NOP, IMP, 2 ) , new Instruction( "???", XXX, IMP, 7 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "CMP", CMP, ABX, 4 ) , new Instruction( "DEC", DEC, ABX, 7 ) , new Instruction( "???", XXX, IMP, 7 ),
                     new Instruction( "CPX", CPX, IMM, 2 ) , new Instruction( "SBC", SBC, IZX, 6 ) , new Instruction( "???", NOP, IMP, 2 ) , new Instruction( "???", XXX, IMP, 8 ) , new Instruction( "CPX", CPX, ZP0, 3 ) , new Instruction( "SBC", SBC, ZP0, 3 ) , new Instruction( "INC", INC, ZP0, 5 ) , new Instruction( "???", XXX, IMP, 5 ) , new Instruction( "INX", INX, IMP, 2 ) , new Instruction( "SBC", SBC, IMM, 2 ) , new Instruction( "NOP", NOP, IMP, 2 ) , new Instruction( "???", SBC, IMP, 2 ) , new Instruction( "CPX", CPX, ABS, 4 ) , new Instruction( "SBC", SBC, ABS, 4 ) , new Instruction( "INC", INC, ABS, 6 ) , new Instruction( "???", XXX, IMP, 6 ),
                     new Instruction( "BEQ", BEQ, REL, 2 ) , new Instruction( "SBC", SBC, IZY, 5 ) , new Instruction( "???", XXX, IMP, 2 ) , new Instruction( "???", XXX, IMP, 8 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "SBC", SBC, ZPX, 4 ) , new Instruction( "INC", INC, ZPX, 6 ) , new Instruction( "???", XXX, IMP, 6 ) , new Instruction( "SED", SED, IMP, 2 ) , new Instruction( "SBC", SBC, ABY, 4 ) , new Instruction( "NOP", NOP, IMP, 2 ) , new Instruction( "???", XXX, IMP, 7 ) , new Instruction( "???", NOP, IMP, 4 ) , new Instruction( "SBC", SBC, ABX, 4 ) , new Instruction( "INC", INC, ABX, 7 ) , new Instruction( "???", XXX, IMP, 7 )
              };
        }

        #region Addressing Modes
        public byte IMP()
        {
            fetched = a;
            return 0;
        }
        public byte IMM()
        {
            addr_abs = pc++;
            return 0;
        }
        public byte ZP0()
        {
            addr_abs = Read(pc);
            pc++;
            addr_abs &= 0x00FF;
            return 0;
        }
        public byte ZPX()
        {
            addr_abs = (byte)(Read(pc) + x);
            pc++;
            addr_abs &= 0x00FF;
            return 0;
        }
        public byte ZPY()
        {
            addr_abs = (byte)(Read(pc) + y);
            pc++;
            addr_abs &= 0x00FF;
            return 0;
        }
        public byte REL()
        {
            addr_rel = Read(pc);
            pc++;

            //TODO Revoir ici. On regarde si le 7th bit (celui qui defini si il est signed ou non) est 1
            if((addr_rel & 0x80) != 0)
            {
                addr_rel |= 0xFF00;
            }

            return 0;
        }
        public byte ABS()
        {
            ushort lo = Read(pc);
            pc++;
            ushort hi = Read(pc);
            pc++;

            addr_abs = (byte)((hi << 8) | lo);

            return 0;
        }
        public byte ABX()
        {
            ushort lo = Read(pc);
            pc++;
            ushort hi = Read(pc);
            pc++;

            addr_abs = (byte)((hi << 8) | lo);
            addr_abs += x;

            //Si besoin de changer de page de mémoire, ajouter un cycle de plus
            if((addr_abs & 0XFF00) != (hi << 8))
            {
                return 1;
            }
            else
            {
                return 0;
            }
        }
        public byte ABY()
        {
            ushort lo = Read(pc);
            pc++;
            ushort hi = Read(pc);
            pc++;

            addr_abs = (byte)((hi << 8) | lo);
            addr_abs += y;

            //Si besoin de changer de page de mémoire, ajouter un cycle de plus
            if ((addr_abs & 0XFF00) != (hi << 8))
            {
                return 1;
            }
            else
            {
                return 0;
            }
        }
        public byte IND()
        {
            ushort ptr_lo = Read(pc);
            pc++;
            ushort ptr_hi = Read(pc);
            pc++;

            ushort ptr = (byte)((ptr_hi << 8) | ptr_lo);

            //Reproduction du bug dans le CPU ou le changement de page ajoute + 1 au l'adresse fetchée
            if(ptr_lo == 0x00FF)
            {
                addr_abs = (byte)(Read((ushort)(ptr & 0xFF00)) << 8 | Read((ushort)(ptr + 0)));
            }
            else
            {
                addr_abs = (byte)((Read((ushort)(ptr + 1)) << 8) | Read((ushort)(ptr + 0)));
            }

            return 0;
        }
        public byte IZX()
        {
            ushort t = Read(pc);
            pc++;

            ushort lo = Read((ushort)((t + x) & 0x00FF));
            ushort hi = Read((ushort)((t + x + 1) & 0x00FF));

            addr_abs = (byte)((hi << 8) | lo);

            return 0;
        }
        public byte IZY()
        {
            ushort t = Read(pc);
            pc++;

            ushort lo = Read((ushort)(t & 0x00FF));
            ushort hi = Read((ushort)(t + 1 & 0x00FF));

            addr_abs = (byte)((hi << 8) | lo);
            addr_abs += y;

            //Si besoin de changer de page de mémoire, ajouter un cycle de plus
            if ((addr_abs & 0xFF00) != (hi << 8))
            {
                return 1;
            }
            else
            {
                return 0;
            }
        }
        #endregion

        #region OPCODES

        #region Storage
        public byte LDA()
        {
            Fetch();
            a = fetched;
            SetFlag(FLAGS.Z, a == 0x00);
            SetFlag(FLAGS.N, (a & 0x80) == 1);
            return 1;
        }
        public byte LDX()
        {
            Fetch();
            x = fetched;
            SetFlag(FLAGS.Z, x == 0x00);
            SetFlag(FLAGS.N, (x & 0x80) == 1);
            return 1;
        }
        public byte LDY()
        {
            Fetch();
            y = fetched;
            SetFlag(FLAGS.Z, y == 0x00);
            SetFlag(FLAGS.N, (y & 0x80) == 1);
            return 1;
        }
        public byte STA()
        {
            Write(addr_abs, a);
            return 0;
        }
        public byte STX()
        {
            Write(addr_abs, x);
            return 0;
        }
        public byte STY()
        {
            Write(addr_abs, y);
            return 0;
        }
        public byte TAX()
        {
            x = a;
            SetFlag(FLAGS.Z, x == 0x00);
            SetFlag(FLAGS.N, (x & 0x80) == 1);
            return 0;
        }
        public byte TAY()
        {
            y = a;
            SetFlag(FLAGS.Z, y == 0x00);
            SetFlag(FLAGS.N, (y & 0x80) == 1);
            return 0;
        }
        public byte TSX()
        {
            x = sp;
            SetFlag(FLAGS.Z, x == 0x00);
            SetFlag(FLAGS.N, (x & 0x80) == 1);
            return 0;
        }
        public byte TXA()
        {
            a = x;
            SetFlag(FLAGS.Z, a == 0x00);
            SetFlag(FLAGS.N, (a & 0x80) == 1);
            return 0;
        }
        public byte TXS()
        {
            sp = x;
            return 0;
        }
        public byte TYA()
        {
            a = y;
            SetFlag(FLAGS.Z, a == 0x00);
            SetFlag(FLAGS.N, (a & 0x80) == 1);
            return 0;
        }
        #endregion
        #region Math
        public byte ADC()
        {
            Fetch();

            ushort temp = (ushort)(a + fetched + GetFlag(FLAGS.C));

            SetFlag(FLAGS.C, temp > 255);
            SetFlag(FLAGS.Z, (temp & 0x00FF) == 0);
            SetFlag(FLAGS.N, (temp & 0x80) == 1);
            SetFlag(FLAGS.V, (~(a ^ fetched) & (a ^ temp) & 0x0080) == 1);

            a = (byte)(temp & 0x00FF);

            return 1;
        }
        public byte DEC()
        {
            Fetch();
            var temp = fetched - 1;
            Write(addr_abs, (byte)(temp & 0x00FF));

            SetFlag(FLAGS.Z, (temp & 0x00FF) == 0x0000);
            SetFlag(FLAGS.N, (temp & 0x0080) == 1);

            return 0;
        }
        public byte DEX()
        {
            x--;
            SetFlag(FLAGS.Z, x == 0x00);
            SetFlag(FLAGS.N, (x & 0x80) == 1);
            return 0;
        }
        public byte DEY()
        {
            y--;
            SetFlag(FLAGS.Z, y == 0x00);
            SetFlag(FLAGS.N, (y & 0x80) == 1);
            return 0;
        }
        public byte INC()
        {
            Fetch();
            var temp = fetched + 1;
            Write(addr_abs, (byte)(temp & 0x00FF));
            SetFlag(FLAGS.Z, (temp & 0x00FF) == 0x0000);
            SetFlag(FLAGS.N, (temp & 0x80) == 1);
            return 0;
        }
        public byte INX()
        {
            x++;
            SetFlag(FLAGS.Z, x == 0x00);
            SetFlag(FLAGS.N, (x & 0x80) == 1);
            return 0;
        }
        public byte INY()
        {
            y++;
            SetFlag(FLAGS.Z, y == 0x00);
            SetFlag(FLAGS.N, (y & 0x80) == 1);
            return 0;
        }
        public byte SBC()
        {
            Fetch();

            ushort value = (byte)(fetched ^ 0x00FF);
            ushort temp = (ushort)(a + value + GetFlag(FLAGS.C));

            SetFlag(FLAGS.C, (temp & 0xFF00) == 1);
            SetFlag(FLAGS.Z, (temp & 0x00FF) == 0);
            SetFlag(FLAGS.V, ( (temp ^ a) & (temp ^ value) & 0x0080 ) == 1);
            SetFlag(FLAGS.N, (temp & 0x0080) == 1);

            a = (byte)(temp & 0x00FF);

            return 1;
        }
        #endregion
        #region Bitwise
        public byte AND()
        {
            Fetch();

            a = (byte)(a & fetched);
            SetFlag(FLAGS.Z, a == 0x00);
            SetFlag(FLAGS.N, (a & 0x80) == 1);

            return 1;
        }
        public byte ASL()
        {
            Fetch();
            var temp = fetched << 1;

            SetFlag(FLAGS.C, (temp & 0xFF00) > 0);
            SetFlag(FLAGS.B, (temp & 0x00FF) > 0);
            SetFlag(FLAGS.N, (temp & 0x80) == 1);

            if (Instructions[opcode].addr == IMP)
            {
                a = (byte)(temp & 0x00FF); 
            }
            else
            {
                Write(addr_abs, (byte)(temp & 0x00FF));
            }

            return 0;
        }
        public byte BIT()
        {
            Fetch();
            var temp = a & fetched;

            SetFlag(FLAGS.Z, (temp & 0x00FF) == 0x00);
            SetFlag(FLAGS.N, (byte)(fetched & (1 << 7)) == 1);
            SetFlag(FLAGS.V, (byte)(fetched & (1 << 6)) == 1);

            return 0;
        }
        public byte EOR()
        {
            Fetch();
            a = (byte)(a ^ fetched);
            SetFlag(FLAGS.Z, a == 0x00);
            SetFlag(FLAGS.N, (a & 0x80) == 1);
            return 1;
        }
        public byte LSR()
        {
            Fetch();

            SetFlag(FLAGS.C, (fetched & 0x0001) == 1);
            var temp = fetched >> 1;
            SetFlag(FLAGS.Z, (temp & 0x00FF) == 0x0000);
            SetFlag(FLAGS.N, (temp & 0x80) == 1);

            if(Instructions[opcode].addr == IMP)
            {
                a = (byte)(temp & 0x00FF);
            }
            else
            {
                Write(addr_abs, (byte)(temp & 0x00FF));
            }

            return 0;
        }
        public byte ORA()
        {
            Fetch();
            a = (byte)(a | fetched);
            SetFlag(FLAGS.Z, a == 0x00);
            SetFlag(FLAGS.N, (a & 0x80) == 1);
            return 1;
        }
        public byte ROL()
        {
            Fetch();
            var temp = (fetched << 1) | GetFlag(FLAGS.C);

            SetFlag(FLAGS.C, (temp & 0xFF00) == 1);
            SetFlag(FLAGS.Z, (temp & 0x00FF) == 0x0000);
            SetFlag(FLAGS.N, (temp & 0x80) == 1);

            if(Instructions[opcode].addr == IMP)
            {
                a = (byte)(temp & 0x00FF);
            }
            else
            {
                Write(addr_abs, (byte)(temp & 0x00FF));
            }

            return 0;
        }
        public byte ROR()
        {
            Fetch();
            var temp = (GetFlag(FLAGS.C) << 7) | (fetched >> 1);

            SetFlag(FLAGS.C, (fetched & 0x01) == 1);
            SetFlag(FLAGS.Z, (temp & 0x00FF) == 0x0000);
            SetFlag(FLAGS.N, (temp & 0x80) == 1);

            if(Instructions[opcode].addr == IMP)
            {
                a = (byte)(temp & 0x00FF);
            }
            else
            {
                Write(addr_abs, (byte)(temp & 0x00FF));
            }

            return 0;
        }
        #endregion
        #region Branch
        public byte BCC()
        {
            if (GetFlag(FLAGS.C) == 0)
            {
                cycles++;
                addr_abs = (byte)(pc + addr_rel);

                if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                {
                    cycles++;
                }

                pc = (byte)addr_abs;
            }

            return 0;
        }
        public byte BCS()
        {
            //TODO voir les flags agissent comment avec le get
            if(GetFlag(FLAGS.C) == 1)
            {
                cycles++;
                addr_abs = (byte)(pc + addr_rel);

                if( (addr_abs & 0xFF00 ) != (pc & 0xFF00))
                {
                    cycles++;
                }

                pc = (byte)addr_abs;
            }

            return 0;
        }
        public byte BEQ()
        {
            if (GetFlag(FLAGS.Z) == 1)
            {
                cycles++;
                addr_abs = (byte)(pc + addr_rel);

                if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                {
                    cycles++;
                }

                pc = (byte)addr_abs;
            }

            return 0;
        }
        public byte BMI()
        {
            if (GetFlag(FLAGS.N) == 1)
            {
                cycles++;
                addr_abs = (byte)(pc + addr_rel);

                if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                {
                    cycles++;
                }

                pc = (byte)addr_abs;
            }

            return 0;
        }
        public byte BNE()
        {
            if (GetFlag(FLAGS.Z) == 0)
            {
                cycles++;
                addr_abs = (byte)(pc + addr_rel);

                if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                {
                    cycles++;
                }

                pc = (byte)addr_abs;
            }

            return 0;
        }
        public byte BPL()
        {
            if (GetFlag(FLAGS.N) == 0)
            {
                cycles++;
                addr_abs = (byte)(pc + addr_rel);

                if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                {
                    cycles++;
                }

                pc = (byte)addr_abs;
            }

            return 0;
        }
        public byte BVC()
        {
            if (GetFlag(FLAGS.V) == 0)
            {
                cycles++;
                addr_abs = (byte)(pc + addr_rel);

                if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                {
                    cycles++;
                }

                pc = (byte)addr_abs;
            }

            return 0;
        }
        public byte BVS()
        {
            if (GetFlag(FLAGS.V) == 1)
            {
                cycles++;
                addr_abs = (byte)(pc + addr_rel);

                if ((addr_abs & 0xFF00) != (pc & 0xFF00))
                {
                    cycles++;
                }

                pc = (byte)addr_abs;
            }

            return 0;
        }
        #endregion
        #region Jump
        public byte JMP()
        {
            pc = (byte)addr_abs;
            return 0;
        }
        public byte JSR()
        {
            pc--;
            Write((ushort)(0x0100 + sp), (byte)((pc >> 8) & 0x00FF));
            sp--;
            Write((ushort)(0x0100 + sp), (byte)(pc & 0x00FF));
            sp--;

            pc = (byte)addr_abs;
            return 0;
        }
        public byte RTI()
        {
            sp++;

            status = Read((ushort)(0x0100 + sp));
            //status &= ~FLAGS.B;
            //status &= ~FLAGS.U;

            sp++;

            pc = Read((ushort)(0x0100 + sp));
            sp++;
            pc |= Read((ushort)((ushort)(0x0100 + sp) << 8));
            return 0;
        }
        public byte RTS()
        {
            sp++;
            pc = Read((ushort)(0x0100 + sp));
            sp++;
            pc |= Read((ushort)((byte)(0x0100 + sp) << 8));

            pc++;

            return 0;
        }
        #endregion
        #region Registers
        public byte CLC()
        {
            SetFlag(FLAGS.C, false);
            return 0;
        }
        public byte CLD()
        {
            SetFlag(FLAGS.D, false);
            return 0;
        }
        public byte CLI()
        {
            SetFlag(FLAGS.I, false);
            return 0;
        }
        public byte CLV()
        {
            SetFlag(FLAGS.V, false);
            return 0;
        }
        public byte CMP()
        {
            Fetch();
            var temp = (byte)(a = fetched);

            SetFlag(FLAGS.C, a >= fetched);
            SetFlag(FLAGS.Z, (temp & 0x00FF) == 0x0000);
            SetFlag(FLAGS.N, (temp & 0x0080) == 1);

            return 1;
        }
        public byte CPX()
        {
            Fetch();
            var temp = (byte)(x - fetched);

            SetFlag(FLAGS.C, x >= fetched);
            SetFlag(FLAGS.Z, (temp & 0x00FF) == 0x0000);
            SetFlag(FLAGS.N, (temp & 0x0080) == 1);

            return 0;
        }
        public byte CPY()
        {
            Fetch();
            var temp = (byte)(y - fetched);

            SetFlag(FLAGS.C, y >= fetched);
            SetFlag(FLAGS.Z, (temp & 0x00FF) == 0x0000);
            SetFlag(FLAGS.N, (temp & 0x0080) == 1);

            return 0;
        }
        public byte SEC()
        {
            SetFlag(FLAGS.C, true);
            return 0;
        }
        public byte SED()
        {
            SetFlag(FLAGS.D, true);
            return 0;
        }
        public byte SEI()
        {
            SetFlag(FLAGS.I, true);
            return 0;
        }
        #endregion
        #region Stack
        public byte PHA()
        {
            Write((ushort)(0x0100 + sp), a);
            sp--;

            return 0;
        }
        public byte PHP()
        {
            //TODO: WHAT IS THAT write(0x0100 + stkp, status | B | U);
            SetFlag(FLAGS.B, false);
            SetFlag(FLAGS.U, false);
            sp--;
            return 0;
        }
        public byte PLA()
        {
            sp++;
            a = Read((ushort)(0x0100 + sp));

            SetFlag(FLAGS.Z, a == 0x00);
            SetFlag(FLAGS.N, (a & 0x80) == 1);

            return 0;
        }
        public byte PLP()
        {
            sp++;
            status = Read((ushort)(0x0100 + sp));
            SetFlag(FLAGS.U, true);
            return 0;
        }
        #endregion
        #region System
        public byte BRK()
        {
            pc++;

            SetFlag(FLAGS.I, true);
            Write((ushort)(0x0100 + sp), (byte)((pc >> 8) & 0x00FF));
            sp--;
            Write((ushort)(0x0100 + sp), (byte)(pc & 0x00FF));
            sp--;

            SetFlag(FLAGS.B, true);
            Write((ushort)(0x0100 + sp), status);
            sp--;
            SetFlag(FLAGS.B, false);

            pc = (byte)(Read(0xFFFE) | (Read(0xFFFF) << 8));

            return 0;
        }
        public byte NOP() 
        {
            switch (opcode)
            {
                case 0x1C:
                case 0x3C:
                case 0x5C:
                case 0x7C:
                case 0xDC:
                case 0xFC:
                    return 1;
            }
            return 0;
        }
        #endregion

        // All other codes
        public byte XXX()
        {
            //TODO: Log something here.
            return 0;
        }

        #endregion

        public void Clock()
        {
            if(cycles == 0)
            {
                opcode = Read(pc);
                pc++;

                cycles = this.Instructions[opcode].cycles;
                byte adressingAdditionalCycle = this.Instructions[opcode].addr();
                byte operationAdditionalCycle = this.Instructions[opcode].opcode();

                cycles += (byte)(adressingAdditionalCycle & operationAdditionalCycle);
            }

            cycles--;
        }

        public void Reset()
        {
            a = 0;
            x = 0;
            y = 0;
            sp = 0xFD;
            status = 0x00;

            addr_abs = 0xFFFC;
            ushort lo = Read((ushort)(addr_abs + 0));
            ushort hi = Read((ushort)(addr_abs + 1));

            pc = (byte)((hi << 8) | lo);

            addr_rel = 0x0000;
            addr_abs = 0x0000;
            fetched = 0x00;

            cycles = 8;
        }

        public void IRQ()
        {
            if (GetFlag(FLAGS.I) == 0)
            {
                Write((ushort)(0x0100 + sp), (byte)((pc << 8) & 0x00FF));
                sp--;
                Write((ushort)(0x0100 + sp), (byte)(pc & 0x00FF));
                sp--;

                SetFlag(FLAGS.B, false);
                SetFlag(FLAGS.U, true);
                SetFlag(FLAGS.I, true);

                Write((ushort)(0x0100 + sp), status);
                sp--;

                addr_abs = 0xFFFE;
                ushort lo = Read((ushort)(addr_abs + 0));
                ushort hi = Read((ushort)(addr_abs + 1));

                pc = (byte)((hi << 8) | lo);

                cycles = 7;
            }
        }

        public void NMI()
        {
                Write((ushort)(0x0100 + sp), (byte)((pc << 8) & 0x00FF));
                sp--;
                Write((ushort)(0x0100 + sp), (byte)(pc & 0x00FF));
                sp--;

                SetFlag(FLAGS.B, false);
                SetFlag(FLAGS.U, true);
                SetFlag(FLAGS.I, true);

                Write((ushort)(0x0100 + sp), status);
                sp--;

                addr_abs = 0xFFFA;
                ushort lo = Read((ushort)(addr_abs + 0));
                ushort hi = Read((ushort)(addr_abs + 1));

                pc = (byte)((hi << 8) | lo);

                cycles = 8;
        }

        private byte Fetch()
        {
            //TODO: Revoir ici pour être certain que IMP est reconnu comme nom de method
            if(!(Instructions[opcode].addr == IMP))
            {
                fetched = Read(addr_abs);
            }

            return fetched;
        }

        public void ConnectBus(Bus bus)
        {
            this.bus = bus;
        }

        public void Write(ushort addr, byte data)
        {
            bus.Write(addr, data);
        }

        public byte Read(ushort addr)
        {
            return bus.Read(addr, false);
        }

        public byte GetFlag(FLAGS f)
        {
            return (byte)(((byte)(status & (byte)f) > 0) ? 1 : 0);
        }

        public void SetFlag(FLAGS f, bool v)
        {
            if (v)
            {
                status |= (byte)f;
            }
            else
            {
                status &= (byte)~(byte)f;
            }
        }
    }
}
