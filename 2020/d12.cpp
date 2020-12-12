#include <string>
#include <iostream>
#include <map>
#include <vector>
#include "tool.cpp"

using namespace std;

static Tool T;
static int wasd[4] = { 0, 0, 0, 0 };
static int dir = 3;

static void print_dir()
{
	cout << "Dir " << dir << endl;
	for (int i = 0; i < 4; i++) {
		cout << wasd[i] << endl;
	}
}

static int manhattan_distance()
{
    int distance = 0;
	for (int i = 0; i < 4; i++) {
		distance += abs(wasd[i]);
	}
    return distance;
}

static int turn_to (int offset)
{
	int current_dir = dir;
	int n = offset / 90;
	dir += n;

	if (dir > 3) {
		dir -= 4;
	} else if (dir < 0) {
		dir += 4;
	}
	//cout << "Turn from " << current_dir << " to " << dir << ", offset " << offset << endl;

	return dir;
}

static int parse_dir(const string& line)
{
	if (line.length() == 0) {
		return 0;
	}

	char d = line[0];
	int offset = stoi(line.substr(1));
	if (d == 'N') {
		wasd[0] += offset - wasd[2];
        wasd[2] = 0;
	} else
	if (d == 'W') {
		wasd[1] += offset - wasd[3];
        wasd[3] = 0;
	} else
	if (d == 'S') {
		wasd[2] += offset - wasd[0];
        wasd[0] = 0;
	} else
	if (d == 'E') {
		wasd[3] += offset - wasd[1];
        wasd[1] = 0;
	} else
	if (d == 'F') {
        int dir_u = (dir + 2) % 4;
		wasd[dir] += offset - wasd[dir_u];
        wasd[dir_u] = 0;
	} else
	if (d == 'R') {
		turn_to(-offset);
	} else
	if (d == 'L') {
		turn_to(offset);
	} else {
		cout << "Unknown move " << line << endl;
	}
    //print_dir();

	return 0;
}

static int wasd_wpt[4] = { 1, 0, 0, 10 };

static void print_dir_wpt()
{
	cout << "Dir WPT" << dir << endl;
	for (int i = 0; i < 4; i++) {
		cout << wasd_wpt[i] << endl;
	}
}

static void rotate_wpt(int offset)
{
	int n = offset / 90;
    int step = 1;

    if (n < 0) step = -1;
    n = abs(n);

    while (n-- > 0) {
        if (step < 0) {
            int dir = wasd_wpt[0];
            int i = 0;

            for (i = 0; i < 3; i++) {
                wasd_wpt[i] = wasd_wpt[i + 1];
            }
            wasd_wpt[i] = dir;
        } else {
            int dir = wasd_wpt[3];
            int i = 3;

            for (i = 3; i > 0; i--) {
                wasd_wpt[i] = wasd_wpt[i - 1];
            }
            wasd_wpt[i] = dir;
        }
    }
}

static int parse_dir_wpt(const string& line)
{
	if (line.length() == 0) {
		return 0;
	}

	char d = line[0];
	int offset = stoi(line.substr(1));
	if (d == 'N') {
		wasd_wpt[0] += offset - wasd_wpt[2];
        wasd_wpt[2] = 0;
	} else
	if (d == 'W') {
		wasd_wpt[1] += offset - wasd_wpt[3];
        wasd_wpt[3] = 0;
	} else
	if (d == 'S') {
		wasd_wpt[2] += offset - wasd_wpt[0];
        wasd_wpt[0] = 0;
	} else
	if (d == 'E') {
		wasd_wpt[3] += offset - wasd_wpt[1];
        wasd_wpt[1] = 0;
	} else
	if (d == 'F') {
        // Add waypoints to wasd
        for (int i = 0; i < 4; i++) {
            int dir_u = (i + 2) % 4;
            if (wasd_wpt[i] != 0 or wasd_wpt[dir_u] != 0) {
		        wasd[i] += wasd_wpt[i] * offset - wasd[dir_u];
                wasd[dir_u] = 0;
            }
        }
	} else
	if (d == 'R') {
		rotate_wpt(-offset);
	} else
	if (d == 'L') {
		rotate_wpt(offset);
	} else {
		cout << "Unknown move " << line << endl;
	}
    //cout << "Move " << line << endl;
    //print_dir();
    //print_dir_wpt();

	return 0;
}

int main (int argc, char *argv[])
{
	Lines lines = T.input(12, -1);
	//T.dump_lines(lines, 10);

    cout << "Part 1" << endl;
	T.fold_vector(lines, parse_dir);
    print_dir();
    cout << "Distance " << manhattan_distance() << endl;

    wasd[0] = 0;
    wasd[1] = 0;
    wasd[2] = 0;
    wasd[3] = 0;

    cout << "Part 2" << endl;
	T.fold_vector(lines, parse_dir_wpt);
    print_dir();
    cout << "Distance2 " << manhattan_distance() << endl;

	return 0;
}