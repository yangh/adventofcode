#include <string>
#include <iostream>
#include "tool.cpp"

using namespace std;

static Tool T;
static int answers[26];
static int people_num = 0;

static void answers_init()
{
	for (int i = 0; i < 26; i++) {
		answers[i] = 0;
	}

	people_num = 0;
}

static int answers_count(bool count_by_people)
{
	int count = 0;

	for (int i = 0; i < 26; i++) {
		if (count_by_people) {
			if (answers[i] == people_num)
				count++;
		} else {
			if (answers[i] > 0)
				count++;
		}
	}

	return count;
}

static int dump_line = 0;

static int answers_check_line(const string & line, bool count_by_people)
{
	int ret = 0;	

	if (line.length() == 0) {
		int sum = answers_count(count_by_people);
		answers_init();
		return sum;
	}

	people_num++;

	for (int i = 0; i < line.length(); i ++) {
		int idx = line[i] - 'a';
		if (dump_line == 1) {
			cout << "Answer: " << line[i] << " idx:" << idx << endl;
		}
		answers[idx] += 1;
	}
	dump_line = 0;

	return 0;
}

static int answers_check_line_p1(const string & line)
{
	return answers_check_line(line, false);
}

static int answers_check_line_p2(const string & line)
{
	return answers_check_line(line, true);
}

int main (int argc, char *argv[])
{
	Lines lines = T.input(6);
	//T.dump_lines(lines, 10);

	int sum = T.fold_vector(lines, answers_check_line_p1);
	cout << "Answer sum " << sum << endl;
	
	int sum2 = T.fold_vector(lines, answers_check_line_p2);
	cout << "Answer sum by everyone " << sum2 << endl;
	
	return 0;
}
