import re, sys
from z3 import Optimize, Int, sat, Sum

def parse_input(input_text):
    machines = []
    lines = input_text.strip().split('\n')

    for line in lines:
        if not line.strip():
            continue

        button_matches = re.findall(r'\(([\d,]+)\)', line)
        buttons = []
        for m in button_matches:
            indices = [int(x) for x in m.split(',')]
            buttons.append(indices)

        target_match = re.search(r'\{([\d,]+)\}', line)
        targets = []
        if target_match:
            targets = [int(x) for x in target_match.group(1).split(',')]

        machines.append({
            'buttons': buttons,
            'targets': targets
        })

    return machines

def solve_machine(machine):
    buttons = machine['buttons']
    targets = machine['targets']
    num_buttons = len(buttons)
    num_targets = len(targets)

    opt = Optimize()

    presses = [Int(f'b_{i}') for i in range(num_buttons)]

    for p in presses:
        opt.add(p >= 0)

    for t_idx in range(num_targets):
        relevant_buttons = []
        for b_idx, button_indices in enumerate(buttons):
            if t_idx in button_indices:
                relevant_buttons.append(presses[b_idx])

        opt.add(Sum(relevant_buttons) == targets[t_idx])

    total_presses = Sum(presses)
    opt.minimize(total_presses)

    if opt.check() == sat:
        model = opt.model()
        return model.eval(total_presses).as_long()
    else:
        return 0

def main():
    input_text = sys.stdin.read()

    machines = parse_input(input_text)

    total_min_presses = 0

    print(f"Analyzing {len(machines)} machines...")
    print("-" * 30)

    for i, machine in enumerate(machines):
        result = solve_machine(machine)
        print(f"Machine {i+1}: Minimum presses = {result}")
        total_min_presses += result

    print("-" * 30)
    print(f"Total Minimum Presses Required: {total_min_presses}")

if __name__ == "__main__":
    main()