#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <string.h>
#include "computer.h"
#undef mips			/* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer (FILE* filein, int printingRegisters, int printingMemory,
  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }
    
    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;

    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
	/*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }

    mips.printingRegisters = printingRegisters;//r
    mips.printingMemory = printingMemory;//-m
    mips.interactive = interactive; //-i
    mips.debugging = debugging; //-d
}

unsigned int endianSwap(unsigned int i) { //shift left << and shift right >> 
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */
void Simulate () {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;
    
    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    int i=0;
    while (i<18) {
        if (mips.interactive) { //checks to see if -i is in interactive mode
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') { //checks to see if it quits 
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);

        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);
        
        // if(instr == 0){
        //     exit(0);
        // }

        /* 
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode (instr, &d, &rVals);

        /*Print decoded instruction*/
        PrintInstruction(&d);

        /* 
	 * Perform computation needed to execute d, returning computed value 
	 * in val 
	 */
        val = Execute(&d, &rVals);
        //printf("new val %d\n", val);

	    UpdatePC(&d,val);

        /* 
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem, 
	 * otherwise put -1 in *changedMem. 
	 * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem);
        

        /* 
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);
        PrintInfo (changedReg, changedMem);
        
        i++;  
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo ( int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
        changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf ("Updated memory at address %8.8x to %8.8x\n",
        changedMem, Fetch (changedMem));
    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR	  CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) {
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch. 
 */
unsigned int Fetch ( int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

/* Decode instr, returning decoded instruction. */
void Decode ( unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
    /*typedef unsigned char BYTE;
    BYTE a = ((instr>>24)&0xFF); 
    printf("Bytes: %02x\n",a);*/
    if(instr == 0){
        exit(0);
    }

    d->op = instr >> 26;//shift to the right 26 bits
    //printf("Instruction %d\n", d->op);
    if(d->op == 0){// r format
        //op|rs|rt|rd|shamt|funct
        d->type = R;
        d->regs.r.rs = instr >> 21 & 0x1f;
        d->regs.r.rt = instr >> 16 & 0x1f;
        d->regs.r.rd = instr >> 11 & 0x1f;
        d->regs.r.shamt = instr >> 6 & 0x1f;
        d->regs.r.funct = instr >> 0 & 0x3f;  
    }
    if((d->op == 9) || (d->op == 12) || (d->op == 15) || (d->op == 13) || (d->op == 4) || (d->op == 5) || (d->op == 35) || (d->op == 43)){//addiu, andi, lui, ori, beq, bne i format
        //printf("Ran i foramt\n");
        //op|rs|rt|immediate
        d->type = I;
            d->regs.i.rs = instr >> 21 & 0x1f;
            d->regs.i.rt = instr >> 16 & 0x1f;
            d->regs.i.addr_or_immed = instr & 0xffff;
            unsigned int leftFirstBit = d->regs.i.addr_or_immed >> 15;
            if(leftFirstBit == 1){
                d->regs.i.addr_or_immed = d->regs.i.addr_or_immed | 0xffff0000;         
            }
        /*printf("rs %d\n", d->regs.i.rs);
        printf("rt %d\n", d->regs.i.rt);
        printf("imm %d\n", d->regs.i.addr_or_immed);*/
        
    }
    if((d->op == 2) || (d->op == 3)){ //jal, j
       //printf("ran j-format\n");
       //op|address
       //printf("Instr %8.8x\n", instr);
        d->type = J;
        d->regs.j.target = instr & 0x3ffffff;
        d->regs.j.target = d->regs.j.target << 2; 
       //printf("Address %d\n", d->regs.j.target);

    }

}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction ( DecodedInstr* d) {
    /* Your code goes here */
    if(d->type == I){
        if(d->op == 9){
            printf("addiu\t$%d, $%d, %d\n", d->regs.i.rt, d->regs.r.rs, d->regs.i.addr_or_immed);
        //addiu}
        }
        else if(d->op == 12){
            printf("andi\t$%d, $%d, $%d\n", d->regs.i.rt, d->regs.r.rs, d->regs.i.addr_or_immed);
        //andi
        }
        else if(d->op == 15){
            printf("lui\t$%d, $%d, $%d\n", d->regs.i.rt, d->regs.r.rs, d->regs.i.addr_or_immed);
        //lui
        }
        else if(d->op == 13){
            printf("ori\t$%d, $%d, $%d\n", d->regs.i.rt, d->regs.r.rs, d->regs.i.addr_or_immed);
        //ori
        }
        else if(d->op == 4){
            //printf("NOW PRINTING IMM %d\n", d->regs.i.addr_or_immed);
            printf("beq\t$%d, $%d, 0x%8.8x\n", d->regs.i.rs, d->regs.r.rt, mips.pc+((d->regs.i.addr_or_immed+1) * 4));
        //beq
        }
        else if(d->op == 5){
            printf("bqe\t$%d, $%d,  0x%8.8x\n", d->regs.i.rs, d->regs.r.rt, mips.pc+((d->regs.i.addr_or_immed+1) * 4));
        //bne
        }
        else if(d->op == 35){
            printf("lw\t $%d $%d(%d)\n", d->regs.i.rt, d->regs.i.addr_or_immed, d->regs.i.rs);
        //lw
        }
        else if(d->op == 43){
            printf("sw\t $%d $%d(%d)\n", d->regs.i.rt, d->regs.i.addr_or_immed, d->regs.i.rs);
        //sw
        }
    }
    if(d->type == R){
        if(d->regs.r.funct == 33){
            printf("addu\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
        //addu}
        }
        else if(d->regs.r.funct == 35){
            printf("subu\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
        //subu}
        }
        else if(d->regs.r.funct == 00){
            printf("sll\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
        //sll}
        }
        else if(d->regs.r.funct == 2){
            printf("srl\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
        //srl}
        }
        else if(d->regs.r.funct == 36){
            printf("and\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
        //and}
        }
        else if(d->regs.r.funct == 37){
            printf("or\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
        //or}
        }
        else if(d->regs.r.funct == 42){
            printf("slt\t$%d, $%d, $%d\n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
        //slt}
        }

        else if(d->regs.r.funct == 8){
            printf("jr\t $%d\n", d->regs.r.rs);
        //jr}
        }
        else{
            exit(0);
        }

    }
    if(d->type == J){
        if(d->op == 3){
            printf("jal\t0x%8.8x\n", d->regs.j.target);
        }
        if(d->op == 2){
            printf("j\t0x%8.8x\n", d->regs.j.target);
        }
    }
}

/* Perform computation needed to execute d, returning computed value */
int Execute ( DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */

    if(d->type == R){ //this checks to see if it is R format and then perform the ALU simulation
        if(d->regs.r.funct == 0){
            //sll
            return (mips.registers[d->regs.r.rt] << mips.registers[d->regs.r.shamt]);
        }
        else if(d->regs.r.funct == 2){
            //srl
            return (mips.registers[d->regs.r.rs] >> mips.registers[d->regs.r.shamt]);
         }
        else if(d->regs.r.funct == 8){
            //jr
            return (mips.registers[31]);
         }
        else if(d->regs.r.funct == 33){
            //addu
            return (mips.registers[d->regs.r.rs] + mips.registers[d->regs.r.rt]);
         }
        else if(d->regs.r.funct == 35){
            //subu
            return (mips.registers[d->regs.r.rs] - mips.registers[d->regs.r.rt]);
         } 
        else if(d->regs.r.funct == 36){
            //and
            return (mips.registers[d->regs.r.rs] &  mips.registers[d->regs.r.shamt]);
         }
        else if(d->regs.r.funct == 37){
            //or
            return (mips.registers[d->regs.r.rs] | mips.registers[d->regs.r.shamt]);
        }
        else if(d->regs.r.funct == 42){
            //slt
            return (mips.registers[d->regs.r.rs] - mips.registers[d->regs.r.shamt] < 0);
        }

    }
    else if(d->type == J){
        //if it in J format, we are going to return the address
        if(d->op == 2){
         //j
             return d->regs.j.target;   
        }
        else if(d->op == 3){
            //jal
            return d->regs.j.target;
        }


    }
    else if(d->type == I){
        if(d->op == 9){
            //addiu
            return (mips.registers[d->regs.i.rs] + d->regs.i.addr_or_immed);
        }
        else if(d->op == 12){
            //andi
            return (mips.registers[d->regs.i.rs] & d->regs.i.addr_or_immed);
        }
        else if(d->op == 4){
            //beq
            return (mips.registers[d->regs.i.rs] - mips.registers[d->regs.i.rt]);
        }
        else if(d->op == 5){
            //bne
            return (mips.registers[d->regs.i.rs] - mips.registers[d->regs.i.rt]);
        }
        else if(d->op == 35){
            //lw
            return (mips.registers[d->regs.i.rs] + d->regs.i.addr_or_immed);
        }
        else if(d->op == 15){
            //lui
            return ((d->regs.i.addr_or_immed << 16 & 0xFFFF0000));
        }
        else if(d->op == 13){
            //ori
            return (mips.registers[d->regs.i.rs] | d->regs.i.addr_or_immed);
        }
        else if(d->op == 43){
            //sw
            return (mips.registers[d->regs.i.rs] + d->regs.i.addr_or_immed);
        }
        
        
    }

  return 0;
}

/* 
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC ( DecodedInstr* d, int val) {

    if(d->type == R){
        //this checks to see if it is R format and then perform the ALU simulation
        if(d->regs.r.funct == 0){
            //sll
            mips.pc+=4;
        }
        else if(d->regs.r.funct == 2){
            //srl
            mips.pc+=4;
         }
        else if(d->regs.r.funct == 8){
            //jr
            mips.pc = val;
         }
        else if(d->regs.r.funct == 33){
            //addu
            mips.pc+=4;
         }
        else if(d->regs.r.funct == 35){
            //subu
            mips.pc+=4;
         } 
        else if(d->regs.r.funct == 36){
            //and
            mips.pc+=4;
         }
        else if(d->regs.r.funct == 37){
            //or
            mips.pc+=4;
        }
        else if(d->regs.r.funct == 42){
            //slt
            mips.pc+=4;
        }   
    }
    if(d->type == I){
        if(d->op == 9){
            //addiu
            mips.pc+=4;
        }
        else if(d->op == 12){
            //andi
            mips.pc+=4;
        }
        else if(d->op == 4){
            //beq
            if(val == 0){
                mips.pc+=((d->regs.i.addr_or_immed+1) * 4);
            }
            else if(val != 0){
               mips.pc+=4; 
            }

        }
        else if(d->op == 5){
            //bne
            if(val == 0){
                mips.pc+=4;
            }
            else if(val!=0){
                mips.pc+=((d->regs.i.addr_or_immed+1) * 4);    
            }
        }
        else if(d->op == 35){
            //lw
            mips.pc+=4;
        }
        else if(d->op == 15){
            //lui
            mips.pc+=4;

        }
        else if(d->op == 13){
            //ori
            mips.pc+=4;

        }
        else if(d->op == 43){
            //sw
            mips.pc+=4;
        }
        
    }
    if(d->type == J){
        if(d->op == 2){
            mips.pc = d->regs.j.target;
        }
        else if(d->op == 3){
            mips.registers[31]=mips.pc+4;//ra contains return adddress
            mips.pc = d->regs.j.target;
        }

    }
    /* Your code goes here */
}

/*
 * Perform memory load or store. Place the address of any updated memory 
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value 
 * that is read, otherwise return -1. 
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory 
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1] 
 * with address 0x00400004, and so forth.
 *
 */
int Mem( DecodedInstr* d, int val, int *changedMem) {
    /* Your code goes here */

    if(d->op == 43){//sw
        if(val < 0x00401000 || val > 0x00404000 || val % 4 != 0){//checks to see if its a valid address
            printf("Memory Access Exception at [0x%08x]: address [0x%08x]", mips.pc, val);
            exit(0);
        }
        else{
            mips.registers[d->regs.i.rt] =  mips.memory[(val - 0x00400000)/4];  
            *changedMem = val;
            val = mips.registers[d->regs.i.rt];
            return val;
        }
    }
    else if(d->op == 35){//lw
        if(val < 0x00401000 || val > 0x00404000 || val % 4 != 0){//checks to see if its a valid address
            printf("Memory Access Exception at [0x%08x]: address [0x%08x]", mips.pc, val);
            exit(0);
        }
        mips.registers[d->regs.r.rt] = mips.memory[(val-0x00400000)/4];
        val = mips.registers[d->regs.r.rt];
        return val;
    }
    else{
        *changedMem = -1;       
        return val;
    }
  return 0;
}

/* 
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    /* Your code goes here */
    if(d->type == I){
        if(d->op == 9){//addiu
            mips.registers[d->regs.i.rt] = val;
            *changedReg = d->regs.i.rt;
        }
        else if(d->op == 12){//andi
            mips.registers[d->regs.i.rt] = val;
            *changedReg = d->regs.i.rt;
        }
        else if(d->op == 13){//ori
            mips.registers[d->regs.i.rt] = val;
            *changedReg = d->regs.i.rt;
        }
        else if(d->op == 15){//lui
            mips.registers[d->regs.i.rt] = val;
            *changedReg = d->regs.i.rt;
        }
        else if((d->op == 4) || (d->op == 5)){//beq & bne
            *changedReg = -1;
        }
        else if(d->op == 35){//lw
            mips.registers[d->regs.i.rt] = val;
            *changedReg = d->regs.i.rt;
        }
        else if(d->op == 43){//sw
            *changedReg = -1;
        }
    }
    if(d->type == R){
        if(d->regs.r.funct == 0){
            //sll
            mips.registers[d->regs.r.rd] = val;
            *changedReg = d->regs.r.rd;
            
        }
        else if(d->regs.r.funct == 2){
            //srl
            mips.registers[d->regs.r.rd] = val;
            *changedReg = d->regs.r.rd;
            
         }
        else if(d->regs.r.funct == 8){
            //jr
            *changedReg = -1;
         }
        else if(d->regs.r.funct == 33){
            //addu
            mips.registers[d->regs.r.rd] = val;
            *changedReg = d->regs.r.rd;
         }
        else if(d->regs.r.funct == 35){
            //subu
            mips.registers[d->regs.r.rd] = val;
            *changedReg = d->regs.r.rd;
         } 
        else if(d->regs.r.funct == 36){
            //and
            mips.registers[d->regs.r.rd] = val;
            *changedReg = d->regs.r.rd;
         }
        else if(d->regs.r.funct == 37){
            //or
            mips.registers[d->regs.r.rd] = val;
            *changedReg = d->regs.r.rd;
        }
        else if(d->regs.r.funct == 42){
            //slt
            mips.registers[d->regs.r.rd] = val;
            *changedReg = d->regs.r.rd;
        }   
    }
    if(d->type == J){
        if(d->op == 2){//j
            *changedReg = -1;
        }
        if(d->op == 3){
            //mips.registers[31] = val;
            *changedReg = 31;// register 31 is holding the new address.
        }
    }
} 
