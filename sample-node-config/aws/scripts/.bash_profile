#!/bin/sh

alias reload='source ~/.bashrc'
alias logs='cat /var/log/cloud-init.log'
alias udlogs='cat /var/log/user-data.log'
alias g=git
alias d=docker
alias dc=docker-compose

function ctag() {
    jq -r .$NETWORK.tag ~/scripts/configure.json
}

function sync() {
    dc -f ~/docker/docker-compose.yaml exec -it cardano-node cardano-cli query tip $(ctag)
}

function up() {
    dc --profile hydraw -f ~/docker/docker-compose.yaml up -d
}

function down() {
    dc --profile hydraw -f ~/docker/docker-compose.yaml down
}

function tui() {
    dc --profile tui -f ~/docker/docker-compose.yaml run hydra-tui
}

function dclean() {
    d stop $1
    d rm $1
}

function hclean() {
    DOCKER_CONTAINER=$(d ps | grep hydra-node | awk '{print $1;}')
    dclean $DOCKER_CONTAINER
}

function hup() {
    dc -f ~/docker/docker-compose.yaml up -d hydra-node
}

function hlogs() {
    dc -f ~/docker/docker-compose.yaml logs -f hydra-node
}

function dcfile() {
    vim ~/docker/docker-compose.yaml
}

function ccli() {
    docker exec -it cardano-node cardano-cli ${@}
}

function balance() {
    ccli query utxo $(ctag) --address $(cat ~/credentials/cardano.addr)
}

function xbalance() {
    ccli query utxo $(ctag) --address $(cat ~/credentials/external.addr)
}

alias fuel='~/scripts/create-fuel.sh'
