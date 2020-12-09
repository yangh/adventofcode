#include <string>
#include <iostream>
#include <map>
#include <vector>
#include "tool.cpp"

using namespace std;

static Tool T;

int main (int argc, char *argv[])
{
	Lines lines = T.input(7);
	T.dump_lines(lines, 10);

	return 0;
}