# Build Variables

EGG_SRC := transmission.scm transmission.utils.scm
EXAMPLE_SRC := example.scm wiki-example.scm
SRC := $(EGG_SRC) $(EXAMPLE_SRC)

default: $(EGG_SRC)
	chicken-install -n

install: $(EGG_SRC)
	chicken-install

test: $(EGG_SRC)
	TEST_USE_ANSI=1 chicken-install -test

clean:
	chicken-clean

lint: $(SRC)
	chicken-lint $(SRC)

test-new-egg:
	test-new-egg transmission https://raw.githubusercontent.com/SiIky/transmission.scm/master/transmission.release-info

watch: $(SRC) transmission.egg
	ls -1 $(SRC) transmission.egg | entr -c make test

# Transmission Daemon Settings

AUTH_NO_AUTH := --auth # --no-auth
CONFIG_DIR := transmission_config
DHT_NO_DHT := --no-dht # --dht
DOWNLOAD_DIR := transmission_download
ENCRYPTION := --encryption-required
HOST := localhost
LOGGING := --log-debug
LPD_NO_LPD := --no-lpd # --lpd
PASSWORD := password
PEER_PORT := 50000
PORTMAP_NO_PORTMAP := --no-portmap # --portmap
RPC_PORT := 4242
USERNAME := username

LOG_FILE := transmission.log

TRANSMISSION_ARGS := \
    $(AUTH_NO_AUTH) \
    $(ENCRYPTION) \
    $(LOGGING) \
    $(PORTMAP_NO_PORTMAP) \
    --config-dir $(CONFIG_DIR) \
    --download-dir $(DOWNLOAD_DIR) \
    --no-incomplete-dir \
    --no-watch-dir \
    --password $(PASSWORD) \
    --peerport $(PEER_PORT) \
    --port $(RPC_PORT) \
    --username $(USERNAME) \
    -f \

CSI_ARGS := \
    --host $(HOST) \
    --password $(PASSWORD) \
    --port $(RPC_PORT) \
    --username $(USERNAME) \

$(DOWNLOAD_DIR):
	mkdir $(DOWNLOAD_DIR)

daemon_running:
	[ -f start-transmission ] # transmission is not running; run `make start-transmission`

csi: default
	csi -setup-mode -R transmission -R transmission.utils

example_repl: default $(EXAMPLE_SRC) daemon_running
	csi -setup-mode -q -s example.scm --repl $(CSI_ARGS)

example: default $(EXAMPLE_SRC) daemon_running
	csi -setup-mode -q -s example.scm $(CSI_ARGS)

start-transmission: $(DOWNLOAD_DIR)
	touch start-transmission
	transmission-daemon $(TRANSMISSION_ARGS) 2>&1 | tee $(LOG_FILE)
	rm start-transmission

.PHONY: clean csi daemon_running default example example_repl install lint test test-new-egg watch
