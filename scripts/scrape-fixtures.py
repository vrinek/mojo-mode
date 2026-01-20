#!/usr/bin/env python3
"""
Scrape syntax-highlighted code snippets from Mojo documentation.

This script uses Playwright to render the docs pages (which use Prism.js
for client-side syntax highlighting) and extracts the token information
to generate test fixtures for mojo-mode.

Usage:
    pip install playwright
    playwright install chromium
    python scripts/scrape-fixtures.py

Output:
    test/fixtures/*.json
"""

import json
import re
import sys
from pathlib import Path

try:
    from playwright.sync_api import sync_playwright
except ImportError:
    print("Please install playwright: pip install playwright")
    print("Then run: playwright install chromium")
    sys.exit(1)


# Pages to scrape
PAGES = [
    {
        "url": "https://docs.modular.com/mojo/manual/get-started",
        "name": "get-started",
    },
    {
        "url": "https://docs.modular.com/mojo/manual/basics",
        "name": "basics",
    },
    {
        "url": "https://docs.modular.com/mojo/manual/functions",
        "name": "functions",
    },
    {
        "url": "https://docs.modular.com/mojo/manual/variables",
        "name": "variables",
    },
    {
        "url": "https://docs.modular.com/mojo/manual/types",
        "name": "types",
    },
    {
        "url": "https://docs.modular.com/mojo/manual/control-flow",
        "name": "control-flow",
    },
    {
        "url": "https://docs.modular.com/mojo/manual/structs",
        "name": "structs",
    },
    {
        "url": "https://docs.modular.com/mojo/manual/traits",
        "name": "traits",
    },
    {
        "url": "https://docs.modular.com/mojo/manual/lifecycle/life",
        "name": "lifecycle-life",
    },
    {
        "url": "https://docs.modular.com/mojo/manual/values/ownership",
        "name": "ownership",
    },
    {
        "url": "https://docs.modular.com/mojo/manual/errors",
        "name": "errors",
    },
    {
        "url": "https://docs.modular.com/mojo/manual/decorators",
        "name": "decorators",
    },
]


def extract_tokens_from_code_block(code_element):
    """Extract token information from a Prism.js highlighted code block."""
    tokens = []
    current_pos = 0

    # Get all text content to build the full code string
    code_text = code_element.inner_text()

    # Find all token spans
    token_spans = code_element.query_selector_all("span[class*='token']")

    for span in token_spans:
        text = span.inner_text()
        if not text:
            continue

        # Extract token type from class (e.g., "token keyword" -> "keyword")
        classes = span.get_attribute("class") or ""
        token_type = None
        for cls in classes.split():
            if cls != "token" and cls.startswith(("keyword", "function", "builtin",
                                                   "string", "number", "operator",
                                                   "punctuation", "comment", "constant",
                                                   "plain")):
                token_type = cls
                break

        if token_type is None:
            # Try to find any class that's not "token" or a generic one
            for cls in classes.split():
                if cls not in ("token", "token-line"):
                    token_type = cls
                    break

        if token_type is None:
            token_type = "plain"

        # Find position in code
        # This is approximate - we search from current position
        try:
            start = code_text.index(text, current_pos)
            end = start + len(text)
            current_pos = end

            tokens.append({
                "start": start,
                "end": end,
                "type": token_type,
                "text": text
            })
        except ValueError:
            # Token text not found at expected position, skip
            pass

    return code_text, tokens


def scrape_page(page, url, page_name):
    """Scrape all code blocks from a documentation page."""
    print(f"Scraping {url}...")
    page.goto(url, wait_until="networkidle")

    # Wait for Prism.js to highlight
    page.wait_for_timeout(2000)

    fixtures = []

    # Find all Mojo code blocks
    code_blocks = page.query_selector_all("div.language-mojo")

    for i, block in enumerate(code_blocks):
        # Get title if present
        title_elem = block.query_selector(".codeBlockTitle_OeMC")
        title = title_elem.inner_text() if title_elem else f"{page_name}-{i}"

        # Get the code element
        code_elem = block.query_selector("code")
        if not code_elem:
            continue

        code_text, tokens = extract_tokens_from_code_block(code_elem)

        if tokens:
            fixtures.append({
                "source": url,
                "title": title,
                "code": code_text,
                "tokens": tokens
            })
            print(f"  Found: {title} ({len(tokens)} tokens)")

    return fixtures


def main():
    output_dir = Path(__file__).parent.parent / "test" / "fixtures"
    output_dir.mkdir(parents=True, exist_ok=True)

    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page()

        for page_info in PAGES:
            fixtures = scrape_page(page, page_info["url"], page_info["name"])

            for fixture in fixtures:
                # Create filename from title
                filename = re.sub(r'[^a-zA-Z0-9_-]', '_', fixture["title"]) + ".json"
                output_path = output_dir / filename

                with open(output_path, "w") as f:
                    json.dump(fixture, f, indent=2)

                print(f"  Wrote: {output_path}")

        browser.close()

    print("\nDone! Run tests with: make test")


if __name__ == "__main__":
    main()
