#! /bin/bash -xe
# Build credentials

# fail if something goes wrong
set -e

DIR=credentials

test -d $DIR || mkdir $DIR

echo "building personal keys"

test -f $DIR/hydra-key.sk ||
    cabal run hydra-node gen-hydra-key

test -f hydra-key.sk &&
    mv hydra-key.sk $DIR

test -f hydra-key.vk &&
    mv hydra-key.vk $DIR

test -f $DIR/cardano-key.sk ||
    cardano-cli address key-gen \
        --signing-key-file "$DIR/cardano-key.sk" \
        --verification-key-file "$DIR/cardano-key.vk"

NETWORK=$(cat terraform.tfvars | awk '{
    if (match($0, /^env[[:space:]]*=[[:space:]]*"([^"]+)"/)) {
        value = substr($0, RSTART + 10, RLENGTH - 10);
        gsub(/^[[:space:]]*=|[[:space:]]+|"/, "", value);
        print value;
    }
}')

export CARDANO_TAG=$(jq -r .$NETWORK.tag ./scripts/configure.json)

test -f $DIR/cardano.addr ||
    cardano-cli address build \
        --payment-verification-key-file "$DIR/cardano-key.vk" \
        $CARDANO_TAG > "$DIR/cardano.addr"

echo "building members keys"

test -d ./$DIR/members || mkdir ./$DIR/members

TEAM_JSON=$(cat ./setup/hydra-team-keys.json | jq)
MEMBERS=$(echo $TEAM_JSON | jq keys | jq -r '.[]')
for member in $MEMBERS; do
    echo "Processing peer: $member"
    MEMBER_JSON=$(echo $TEAM_JSON  | jq ".$member")

    cardanoVK=$(echo $MEMBER_JSON | jq '.["cardano-vk"]')
    cat << EOF >./$DIR/members/$member.cardano.vk
{
    "type": "PaymentVerificationKeyShelley_ed25519",
    "description": "Payment Verification Key",
    "cborHex": $cardanoVK
}
EOF

    hydraVK=$(echo $MEMBER_JSON | jq '.["hydra-vk"]')
    cat << EOF >./$DIR/members/$member.hydra.vk
{
    "type": "HydraVerificationKey_ed25519",
    "description": "",
    "cborHex": $hydraVK
}
EOF
done
