
# The purpose of this script is to execute tests and compare with expected results.
# For now, only a single execution is checked...

import subprocess

program_name = "../../build/jt65code"
process = subprocess.Popen([program_name, "Hello"], stdout=subprocess.PIPE, universal_newlines=True)
actual_result = ""
for line in process.stdout:
    actual_result += line

expected_name = "../expected-hello.txt"
with open(expected_name) as input_file:
    expected_result = input_file.read()

print("ACTUAL OUTPUT:")
print("----------")
print(actual_result)
print("----------\n")

print("EXPECTED OUTPUT:")
print("----------")
print(expected_result)
print("----------\n")

print("Do they agree?", actual_result == expected_result)
