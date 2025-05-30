import sys
import re


def parse_imports(input_text):
    dependencies = {}
    lines = input_text.strip().split("\n")
    for line in lines:
        match = re.match(r"src/([\w/]+)\.hs:import\s+([\w\.]+)", line)
        if match:
            module_path, imported_module = match.groups()
            module = module_path.replace("/", ".")
            dependencies.setdefault(module, []).append(imported_module)
    return dependencies


def generate_dot(dependencies):
    dot_output = ["digraph G {"]
    for module, imports in dependencies.items():
        for imported_module in imports:
            dot_output.append(f'    "{module}" -> "{imported_module}";')
    dot_output.append("}")
    return "\n".join(dot_output)


# Usage:
# grep '^import' src/Malgo/**/*.hs | python graph.py > dependencies.dot
if __name__ == "__main__":
    if len(sys.argv) > 1:
        with open(sys.argv[1], "r") as file:
            input_text = file.read()
    else:
        input_text = sys.stdin.read()

    dependencies = parse_imports(input_text)
    dot_output = generate_dot(dependencies)
    print(dot_output)
