import re
from os.path import join

def parse_description(pkg_path):
    description_content = open(join(pkg_path,"DESCRIPTION"),"r").read()
    """Parse the DESCRIPTION content to extract package dependencies."""
    dependencies = []
    # Regular expressions to match the fields
    field_pattern = re.compile(r'^([A-Za-z0-9/]+):\s*(.*)')
    continuation_pattern = re.compile(r'^\s+(.*)')
    current_field = None
    fields_of_interest = ['Imports', 'Suggests', 'LinkingTo']
    field_values = {key: '' for key in fields_of_interest}
    lines = description_content.split('\n')
    for line in lines:
        line = line.rstrip('\n')
        if line == '':
            current_field = None
            continue
        field_match = field_pattern.match(line)
        if field_match:
            key = field_match.group(1)
            value = field_match.group(2).strip()
            if key in fields_of_interest:
                current_field = key
                field_values[current_field] += ' ' + value if field_values[current_field] else value
            else:
                current_field = None
        else:
            continuation_match = continuation_pattern.match(line)
            if continuation_match and current_field:
                continuation = continuation_match.group(1)
                field_values[current_field] += ' ' + continuation.strip()
    # Now parse the field values to extract package names
    for field in fields_of_interest:
        if field_values[field]:
            # Remove comments and split by commas
            entries = re.sub(r'#.*', '', field_values[field]).split(',')
            for entry in entries:
                pkg = entry.strip()
                if pkg:
                    # Remove version specifications in parentheses
                    pkg_name = re.split(r'\s*\(.*\)', pkg)[0].strip()
                    dependencies.append(pkg_name)
    # Remove duplicates while preserving order
    seen = set()
    dependencies = [x for x in dependencies if not (x in seen or seen.add(x))]
    return dependencies

