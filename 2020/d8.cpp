#include <string>
#include <iostream>
#include <map>
#include <vector>
#include "tool.cpp"

using namespace std;

static Tool T;

struct Instr {
    string name;
    int opcode;
    int param1;
    // Statistics
    int niteration;
};

struct RedWhiteMachine {
    int pc;
    int acc;
};

static RedWhiteMachine rwm { 0, 0, };

static vector<Instr> codes;
const static string instr_delim = " ";

typedef int (*opfunc) (int);

typedef struct OpCode {
    int opcode;
    opfunc func;
} OpCode;

typedef map<string, OpCode> instrMap;

static int acc(int param1)
{
    rwm.acc += param1;
    rwm.pc++;
    return rwm.acc;
}

static int nop(int param1)
{
    rwm.pc++;
    return 0;
}

static int jmp(int param1)
{
    rwm.pc += param1;
    return rwm.pc;
}

const static int OP_ACC = 0;
const static int OP_JMP = 1;
const static int OP_NOP = 2;

static instrMap instr_map {
    { "acc", { OP_ACC, acc, }, },
    { "jmp", { OP_JMP, jmp, }, },
    { "nop", { OP_NOP, nop, }, },
};

static int instrNameToOpCode(const string& name)
{
    instrMap::iterator iter = instr_map.find(name);
    
    if (iter != instr_map.end()) {
        return iter->second.opcode;
    }

    return -1;
}

static int parse_line(const string& line)
{
    Instr instr;
    vector<string> tokens = T.string_split(line, instr_delim);

    if (tokens.size() >= 2) {
        instr.name = tokens[0];
        instr.param1 = stoi(tokens[1]);
        instr.opcode = instrNameToOpCode(instr.name);
        instr.niteration = 0;
        codes.push_back(instr);
        //cout << "New instr " << instr.name << ", " << instr.opcode << ", " << instr.param1 << endl;

        return 1;
    }

    return 0;
}

static int run_till_first_loop(int niteration_max = 1)
{
    rwm.acc = 0;
    rwm.pc = 0;
    int code_end = codes.size();

    for (auto &instr : codes) {
        instr.niteration = 0;
    }

    while (1) {
        if (rwm.pc == code_end) {
            cout << "Reach the exact end of the code, PC " << rwm.pc << ", Acc " << rwm.acc << endl;
            rwm.acc = -2;
            break;
        } else if (rwm.pc > code_end || rwm.pc < 0) {
            cout << "Reach out range of the code, PC " << rwm.pc << endl;
            rwm.acc = -1;
            break;
        }

        Instr& instr = codes[rwm.pc];

        // Set the max loop time
        if (instr.niteration == niteration_max) {
            instr.niteration = 0;
            break;
        }

        instr.niteration++;

        instrMap::iterator iter = instr_map.find(instr.name);
        if (iter != instr_map.end()) {
            OpCode & op = iter->second;
            int ret = op.func(instr.param1);
            //cout << "Exec " << instr.name << ", " << instr.param1 << ", pc " << rwm.pc << endl;
        } else {
            cerr << "Invalid instruction " << instr.name << endl;
            rwm.acc = -1;
        }
    }

    return rwm.acc;
}

static void correct_infinit_loop()
{
    string orig_name;
    int orig_op;

    for (auto &instr : codes) {
        orig_name = instr.name;
        orig_op = instr.opcode;

        if (instr.opcode == OP_JMP) {
            instr.name = "nop";
            instr.opcode = OP_NOP;
        } else if (instr.opcode == OP_NOP) {
            instr.name = "jmp";
            instr.opcode = OP_JMP;
        } else {
            continue;
        }

        int ret = run_till_first_loop(500);

        //cout << "Check " << orig_name << ", param " << instr.param1 << ", ret " << ret << ", acc " << rwm.acc << ", pc " << rwm.pc << endl;
        if (ret > 0) {
            //cout << "It's a loop" << endl;
            ;
        } else if (ret == -2) {
            cout << "Found" << endl;
            break;
        } else if (ret == -1) {
            cout << "ERROR" << endl;
            break;
        }

        instr.name = orig_name;
        instr.opcode = orig_op;
    }
}

int main (int argc, char *argv[])
{
	Lines lines = T.input(8);
	//T.dump_lines(lines, 10);

    cout << "Instructions " << T.fold_vector(lines, parse_line) << endl;
    cout << "Acc value at the first loop " << run_till_first_loop(1) << endl;
    correct_infinit_loop();

	return 0;
}