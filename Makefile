.PHONY: test clean install scrape-fixtures

test:
	cask exec buttercup -L . test/

clean:
	rm -rf .cask

install:
	cask install

# Scrape test fixtures from Mojo docs (requires playwright)
# Install: pip install playwright && playwright install chromium
scrape-fixtures:
	python3 scripts/scrape-fixtures.py
