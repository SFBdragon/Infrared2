from os import listdir
from os.path import isfile, join

INPUT_PATH = "D:\\temp\\pgns\\"
OUTPUT = "D:\\temp\\twic.pgn"

def main():
    with open(OUTPUT, 'w') as output:
        for name in listdir(INPUT_PATH):
            if isfile(join(INPUT_PATH, name)):
                if ".pgn" in name:
                    with open(join(INPUT_PATH, name)) as pgn:
                        try:
                            output.write(pgn.read())
                        except:
                            continue
                            

if __name__ == "__main__":
    main()