#include <string>
#include <iostream>
#include <map>
#include <vector>
#include "tool.cpp"

using namespace std;

static Tool T;

static int parse_line(const string& line)
{
	return 0;
}

int main (int argc, char *argv[])
{
	Lines lines = T.input(7);
	T.dump_lines(lines, 10);

    cout << "Part 1" << endl;
	T.fold_vector(lines, parse_line);

	return 0;
}