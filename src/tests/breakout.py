import argparse
import curses as c
import subprocess as s

def play(manual, naptime):
    try:
        screen = c.initscr()
        y, x = screen.getmaxyx()
        if y < 24 or x < 80:
            print("Terminal must be at least 80x24, aborting.")
            quit()
        c.curs_set(0)
        c.noecho()

        to_tile = {0:' ',
                1:'#',
                2:'_',
                3:'^',
                4:'@'}
        to_move = {104:-1,
                46:0,
                108:1}

        ball_pos = (0,0)
        ball_dir = 1
        paddle_pos = 0 # just an x coord
        scores = []
        tiles = {}
        cmd = ["stack", "exec", "intcode",
               "/home/umer/src/intcode/src/tests/sample_inputs/input13v2.txt"]
        p = s.Popen(cmd, stdout=s.PIPE, stdin=s.PIPE, stderr=s.PIPE, text=True)

        if manual:
            screen.addstr(20, 50, "'h' to move left")
            screen.addstr(21, 50, "'l' to move right")

        while True:
            out = p.stdout.readline().strip()
            if out == "Done":
                break
            if out == "Waiting for input...":
                screen.refresh()
                if manual:
                    inp = ''
                    while not inp:
                        inp = to_move.get(screen.getch(), '')
                else:
                    # I doubt this is a perfect strategy, but it's good enough
                    c.napms(naptime)
                    if ball_pos[1] > paddle_pos:
                        inp = 1
                    elif ball_pos[1] < paddle_pos:
                        inp = -1
                    elif tiles[ball_pos[0], ball_pos[1] + ball_dir] == 2:
                        inp = -ball_dir
                    else:
                        inp = ball_dir

                p.stdin.write(str(inp) + "\n")
                p.stdin.flush()
                continue
            else: # so it's a number
                x = int(out)
                y = int(p.stdout.readline().strip())
                tile_id = int(p.stdout.readline().strip())
                tiles[(y,x)] = tile_id
                if tile_id == 3:
                    paddle_pos = x
                if tile_id == 4:
                    ball_dir = 1 if x - ball_pos[1] > 0 else -1
                    ball_pos = (y, x)
                if x == -1:
                    scores += [tile_id]
                    screen.addstr(0, 50, "          ") # clear old score
                    screen.addstr(0, 50, str(tile_id))
                else:
                    screen.addch(y, x, to_tile[tile_id])

        score = max(scores)
        screen.addstr(0, 50, "          ") # clear old score
        screen.addstr(0, 50, "Final score: ")
        screen.addstr(1, 50, str(score))
        screen.addstr(3, 50, "Press any key to quit...")
        screen.getch()

    finally:
        c.curs_set(1)
        c.echo()
        c.endwin()

if __name__ == "__main__":
    arg_parser = argparse.ArgumentParser()
    help_str = "-h, --help  show this message and exit"
    arg_parser.add_argument("-m", "--manual", default=False, type=bool,
                            help="play manually")
    arg_parser.add_argument("-t", "--time", default=5, type=int,
        help="Pause between moves when playing automatically, in ms, default 5")
    args = arg_parser.parse_args()
    play(args.manual, args.time)
