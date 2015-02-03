def cleanup_alias():
    filename = '../savefile/eshell/alias'
    content = ""


    with open(filename, 'r') as ins:
        line = ins.readline()
        while line:
            seg = line.strip().split()

            if seg[1] == 'alias':
                line = ' '.join(seg[1:])

            content += line + '\n'
            line = ins.readline()

    with open(filename, 'w') as out:
        out.write(content)

        for i in xrange(3):
            print "".format(3)

if __name__ == '__main__':
    cleanup_alias()
