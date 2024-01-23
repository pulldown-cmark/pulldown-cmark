TARGET=$(shell cat target.txt)

.PHONY: lint
lint:
	@cargo +nightly clippy

.PHONY: fix
fix:
	@cargo +nightly clippy --fix

.PHONY: test
test:
	@cargo +nightly test

.PHONY: shellspec
shellspec:
	@shellspec

.PHONY: install
install:
	@cargo +nightly install --path .

.PHONY: check
check:
	@cargo +nightly check

.PHONY: target-add
target-add:
	@rustup override set nightly
	@rustup target add $(TARGET)

.PHONY: target-build
target-build: target-add
	@for target in $(TARGET); do echo $$target; cargo +nightly zigbuild --release --target $$target || cargo +stable zigbuild --release --target $$target; done

.PHONY: target-archive
target-archive: target-build
	@bash archive.sh $(TARGET)
