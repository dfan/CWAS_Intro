# MySQL works best without quotes around strings
# did some pre-processing already on the raw textfile (replaced apostrophes with blanks)
original = open('Phewas_rawcode.txt', 'r');
new = open('Phewas_code.txt', 'w');

# skip header row
new.write(original.readline().decode("utf-8").rstrip('\n\r'));
new.write("\n");
for line in original.readlines():
    split = line.decode("utf-8").rstrip('\n\r').split('\t');
    # if male, fill with 0 and if female, fill with 1
    if split[7] == "Male":
        split[7] = 0;
    if split[7] == "Female":
        split[7] = 1;    
    # if empty entry for sex, fill with -1
    if split[7] == "":
        split[7] = -1;

    # 10 exclusive (1 to 9)
    for i in range(0, 10):
        new.write(str(split[i]) + "\t");
    new.write(str(split[10]));
    new.write("\n");