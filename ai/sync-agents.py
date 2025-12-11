"""Install Spacemacs AI Agents for GitHub Copilot and Gemini CLI."""

import os
import re

# ==========================================
#  SPACEMACS AGENT BUILDER (V17.5 - COPILOT TOOLBOX)
# ==========================================
# BASE: V17 (Stable)
# NEW: Adds textual "REQUIRED TOOLBOX" instruction to Copilot agents,
#      telling them which profile to load (similar to Gemini's !cat).
# ==========================================

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = ".github"
GEMINI_CMD_DIR = os.path.join(".gemini", "commands")

SOURCES = [
    {
        "file": "coding_ai.md",
        "marker": "### The Specialist Team Roster",
        "type": "specialist"
    },
    {
        "file": "general_ai.md",
        "marker": "### Default Universal Persona",
        "type": "strategic",
        "footer_pattern": r"(?m)^## 5\. How to Choose.*"
    },
    {
        "file": "stakeholder_ai.md",
        "marker": "## 1. The Core User Base (The Community)",
        "type": "simulation"
    }
]

NAME_MAPPING = {
    "professor": "professor",
    "mckarthy": "professor",
    "kael": "kaelthas",
    "bob": "bob",
    "lector": "lector",
    "freud": "freud",
    "griznak": "griznak",
    "orb": "orb",
    "magos": "magos",
    "scribe": "scribe",
    "reginald": "reginald",
    "kallista": "kallista",
    "spacky": "spacky",
    "bzzrts": "bzzrts",
    "vala": "vala",
    "nexus": "nexus",
    "marjin": "marjin",
    "dok": "dok",
    "golem": "golem",
    "skeek": "skeek",
    "don": "don",
    "chen": "chen",
    "vlad": "vlad",
    "rms": "rms",
    "noobie": "noobie",
    "sarah": "sarah"
}

PROFILE_MAP = {
    "spacky": "ai/profile_elisp.md",
    "bzzrts": "ai/profile_emacs_ui.md",
    "nexus": "ai/profile_layers.md",
    "vala": "ai/profile_ci_github.md",
    "don": "ai/profile_elisp_testing.md",
    "golem": "ai/profile_doc.md"
}

def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)

def clean_slug(name):
    name_lower = name.lower()
    for key, slug in NAME_MAPPING.items():
        if key in name_lower:
            return slug
    return name_lower.split()[0].replace(".", "").replace("'", "").strip()

def get_mode_text(agent_type):
    if agent_type == "strategic":
        return """
MODE: STRATEGIC PLANNING & ARCHITECTURE
(Focus on high-level design, user stories, and requirements. Use Github MCP if available to read issues.)
"""
    elif agent_type == "simulation":
        return """
MODE: USER SIMULATION
(Focus on subjective feedback, usability, and constraints. Do not write code.)
"""
    elif agent_type == "specialist":
        return """
MODE: IMPLEMENTATION & CRAFTSMANSHIP
(Focus on concrete code, strict rules, and technical correctness. Adhere to the loaded profile.)
"""
    return ""

def get_model_id(agent_type):
    if agent_type == "specialist":
        return "gpt-5.1-codex"
    else:
        return "gpt-5.1"

def clean_header_content(header):
    # Removes trailing dashes/stars aggressively
    cleaned = re.sub(r'(\n\s*[-*]{3,}\s*)+$', '', header.strip())
    return cleaned.strip()

def parse_agents_from_text(roster_content, source_type):
    agents = []
    raw_splits = re.split(r"(?m)^-\s+\*\*(Role|Name):\*\*\s+", roster_content)

    if len(raw_splits) < 2:
        return agents

    iterator = iter(raw_splits[1:])
    for key, chunk in zip(iterator, iterator):
        role = "Unknown"
        name = "Unknown"

        chunk = re.split(r"(?m)^### ", chunk)[0]

        if key == "Role":
            role = chunk.split("\n")[0].strip()
            name_match = re.search(r"-\s+\*\*Name:\*\*\s+(.*?)$", chunk, re.MULTILINE)
            name = name_match.group(1).strip() if name_match else "Unknown"
        elif key == "Name":
            name = chunk.split("\n")[0].strip()
            role_match = re.search(r"-\s+\*\*Role:\*\*\s+(.*?)$", chunk, re.MULTILINE)
            role = role_match.group(1).strip() if role_match else "Simulation Persona"

        slug_name = clean_slug(name)
        full_body = f"- **{key}:** {chunk.strip()}"

        agents.append({
            "name": name,
            "slug": slug_name,
            "role": role,
            "body": full_body,
            "type": source_type
        })
    return agents

def generate_copilot_files(global_headers, agents):
    print(f"📝 Generating GitHub Copilot Agents in {BASE_DIR}/agents/...")
    agents_dir = os.path.join(BASE_DIR, "agents")
    ensure_dir(agents_dir)

    for agent in agents:
        filename = f"{agent['slug']}.agent.md"
        path = os.path.join(agents_dir, filename)
        target_model = get_model_id(agent["type"])

        yaml = f"---\nname: {agent['slug']}\ndescription: {agent['role']}\nmodel: {target_model}\n---"
        context = global_headers.get(agent["type"], "")
        mode_text = get_mode_text(agent["type"])

        # NEW: Toolbox Logic for Copilot
        slug = agent["slug"]
        profile_path = PROFILE_MAP.get(slug)
        toolbox_text = ""

        if profile_path:
            toolbox_text = f"\n\n---\n**REQUIRED TOOLBOX**\nThis agent requires specific technical rules. Please automatically load or reference the content of:\n`{profile_path}`\n"
        elif agent["type"] == "specialist":
            toolbox_text = "\n\n---\n**REQUIRED TOOLBOX**\nNo specific profile assigned. If implementation is needed, ask the user to load the appropriate `profile_*.md`.\n"

        # Structure: YAML -> Context -> Identity -> Toolbox (New) -> Mode
        content = f"{yaml}\n\n{context}\n\n---\n\n# Identity: {agent['name']}\n{agent['body']}{toolbox_text}\n\n---\n{mode_text}"

        with open(path, "w", encoding="utf-8") as f:
            f.write(content)

    print(f"   Generated {len(agents)} agent files.")

def generate_gemini_commands(global_headers, agents):
    print(f"💎 Generating Gemini CLI Commands in {GEMINI_CMD_DIR}...")
    ensure_dir(GEMINI_CMD_DIR)

    for agent in agents:
        slug = agent["slug"]
        profile_path = PROFILE_MAP.get(slug)
        mode_section = get_mode_text(agent["type"])

        toolbox_section = ""
        if profile_path:
            toolbox_section = f"\nTOOLBOX (AUTO-LOADED):\n!{{cat {profile_path}}}\n"
        elif agent["type"] == "specialist":
             toolbox_section = "\nTOOLBOX:\n(No specific profile loaded. Ask user to load one if implementation is needed.)\n"

        system_header = global_headers.get(agent["type"], "")

        # Clean prompt, removed extra --- lines
        prompt_text = f"""
SYSTEM INSTRUCTIONS:
{system_header}

---
AGENT PERSONA:
{agent['body']}

---
{mode_section}
{toolbox_section}
---
USER INPUT:
{{{{args}}}}
"""
        clean_desc = agent['role'].replace('"', "'")
        toml_content = f'description = "{clean_desc}"\n'
        toml_content += 'prompt = """' + prompt_text + '"""\n'

        filename = f"{slug}.toml"
        path = os.path.join(GEMINI_CMD_DIR, filename)

        with open(path, "w", encoding="utf-8") as f:
            f.write(toml_content)

    print(f"   Generated {len(agents)} commands.")

def main():
    all_agents = []
    global_headers = {}

    for source in SOURCES:
        file_path = os.path.join(SCRIPT_DIR, source["file"])
        if not os.path.exists(file_path):
            print(f"❌ Error: {file_path} not found.")
            continue

        print(f"🚀 Reading {source['file']}...")
        with open(file_path, "r", encoding="utf-8") as f:
            full_content = f.read()

        if source["marker"] not in full_content:
            print(f"⚠️ Warning: Marker '{source['marker']}' not found in {source['file']}.")
            continue

        # Split Header and Roster
        parts = full_content.split(source["marker"])
        header_raw = parts[0]
        roster_raw = parts[1]

        header = clean_header_content(header_raw)

        # Handle Footer
        if "footer_pattern" in source:
            footer_match = re.search(source["footer_pattern"], roster_raw, re.DOTALL)
            if footer_match:
                print(f"   ℹ️  Found Footer (Phonebook) in {source['file']}.")
                split_index = footer_match.start()
                footer_content = roster_raw[split_index:]
                roster_raw = roster_raw[:split_index]
                header = header + "\n\n---\n" + footer_content.strip()
            else:
                print(f"   ⚠️  Footer pattern defined but not found in {source['file']}.")

        global_headers[source["type"]] = header

        agents = parse_agents_from_text(roster_raw, source["type"])
        all_agents.extend(agents)
        print(f"   Found {len(agents)} agents.")

    generate_copilot_files(global_headers, all_agents)
    generate_gemini_commands(global_headers, all_agents)

    print("\n✅ Done! Unified Framework Active.")

if __name__ == "__main__":
    main()
