
# The purpose of this script is to execute tests and compare with expected results.
# For now, only a single execution is checked...

import subprocess


class TestCase:
    def __init__(self, case_command, case_argument, case_result_file):
        self.command = case_command
        self.argument = case_argument
        self.result_file = case_result_file


test_cases = [TestCase("../build/jt65code", "Hello", "expected-hello.txt"),
              TestCase("../build/jt65code", "-t", "expected-test.txt"),
              TestCase("../build/check_pack", "", "expected-check-pack.txt")]
overall_success = True

for test_case in test_cases:
    process = subprocess.Popen([test_case.command, test_case.argument], stdout=subprocess.PIPE, universal_newlines=True)
    actual_result = ""
    for line in process.stdout:
        actual_result += line

    with open(test_case.result_file) as input_file:
        expected_result = input_file.read()
    # expected_result.replace('\r', '')   # Not really needed?

    # It might be nice to have an option to suppress this output.
    print("\nACTUAL OUTPUT:")
    print("#####")
    print(actual_result)
    print("#####\n")

    print("EXPECTED OUTPUT:")
    print("#####")
    print(expected_result)
    print("#####", end="  ")

    # TODO: Give test cases names and print the name here.
    case_success = actual_result == expected_result
    print("Do they agree?", case_success)
    overall_success = overall_success and case_success

# The final assessment...
if overall_success:
    print("PASS!")
else:
    print("FAIL!")
