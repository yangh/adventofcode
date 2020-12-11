#include <string>
#include <iostream>
#include <map>
#include <vector>
#include "tool.cpp"

using namespace std;

static Tool T;
static vector<vector<int>> seats;
static vector<vector<int>> seats_shadow;

static int FLOOR = 0;
static int EMPTY = 1;
static int OCCED = 2;
static int width = 0;
static int height = 0;

static void parse_lines(Lines& lines)
{
    width = 0;
    height = 0;

    for (const auto& line: lines) {
        vector<int> seat_line;
        vector<int> seat_shadow;
        if (line.length() > 0) {
            width = line.length();
            height++;

            for (int i = 0; i < width; i++) {
                char c = line[i];

                if (c == '.') {
                    seat_line.push_back(FLOOR);
                } else 
                if (c == 'L') {
                    seat_line.push_back(EMPTY);
                } else 
                if (c == '#') {
                    seat_line.push_back(OCCED);
                } 
                seat_shadow.push_back(0);
            }
            seats.push_back(seat_line);
            seats_shadow.push_back(seat_shadow);
        }
    }
    //cout << "WxH " << width << " " << height << endl;
}

static int xy_offset[8][2] = {
   { -1, -1 }, {  0, -1 }, {  1, -1 },
   { -1,  0 },             {  1,  0 },
   { -1,  1 }, {  0,  1 }, {  1,  1 },
};

static int count_seat_around(int x, int y, int type)
{
    int x1, y1;
    int count = 0;

    for (int i = 0; i < 8; i++) {
        x1 = x + xy_offset[i][0];
        y1 = y + xy_offset[i][1];
        if (x1 < 0 || x1 >= width) continue;
        if (y1 < 0 || y1 >= height) continue;
        if (type == seats[y1][x1]) {
            count++;
        }
    }
    return count;
}

static int count_seat_around2(int x, int y, int type)
{
    int x1, y1;
    int count = 0;

    for (int i = 0; i < 8; i++) {
        x1 = x;
        y1 = y;
        do {
            x1 += xy_offset[i][0];
            y1 += xy_offset[i][1];
            if (x1 < 0 || x1 >= width) break;
            if (y1 < 0 || y1 >= height) break;
            if (type == seats[y1][x1]) {
                count++;
                break;
            } else if (FLOOR == seats[y1][x1]) {
                continue;
            } else {
                break;
            }
        } while (1);
    }
    return count;
}

static void dump_seats()
{
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            cout << seats[y][x];
        }
        cout << endl;
    }
}

static int lifecycle(int mode, int occnum)
{
    bool state_changed = false;
    int round = 0;

    do {
        state_changed = false;
        round++;

        //cout << "Lifecycle round " << round << endl;
        //dump_seats();

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                int c = seats[y][x];
                int occed = 0;
                if (mode == 1) {
                    occed = count_seat_around(x, y, OCCED);
                } else {
                    occed = count_seat_around2(x, y, OCCED);
                }

                seats_shadow[y][x] = c;

                if (c == EMPTY && occed == 0) {
                    seats_shadow[y][x] = OCCED;
                    state_changed = true;
                } else 
                if (c == OCCED && occed >= occnum) {
                    seats_shadow[y][x] = EMPTY;
                    state_changed = true;
                }
            }
        }

        // Copy shodow to seats
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                seats[y][x] = seats_shadow[y][x];
            }
        }
    } while (state_changed);

    int count = 0;
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if (OCCED == seats[y][x]) {
                count++;
            }
        }
    }

    return count;
}

int main (int argc, char *argv[])
{
	Lines lines = T.input(11, -1);
	//T.dump_lines(lines, 10);

    parse_lines(lines);
    cout << "Lifecycle occupped " << lifecycle(1, 4) << endl;

    seats.clear();
    seats_shadow.clear();
    parse_lines(lines);
    cout << "Lifecycle occupped2 " << lifecycle(2, 5) << endl;

	return 0;
}