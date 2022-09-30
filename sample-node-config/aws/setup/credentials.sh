#! /bin/bash -xe
# Build credentials

# fail if something goes wrong
set -e

DIR=credentials

test -d $DIR || mkdir $DIR

echo "building personal keys"

test -f $DIR/hydra-key.sk || 
    cabal run hydra-tools gen-hydra-key

test -f hydra-key.sk &&
    mv hydra-key.sk $DIR

test -f hydra-key.vk &&
    mv hydra-key.vk $DIR

test -f $DIR/cardano-key.sk || 
    cardano-cli address key-gen \
        --signing-key-file "$DIR/cardano-key.sk" \
        --verification-key-file "$DIR/cardano-key.vk"

test -f $DIR/cardano.addr ||
    cardano-cli address build \
        --payment-verification-key-file "$DIR/cardano-key.vk" \
        --testnet-magic 1 > "$DIR/cardano.addr"

echo "building members keys"

TEAM_JSON=$(cat ./setup/hydra-team-keys.json | jq)
MEMBERS=$(echo $TEAM_JSON | jq keys | jq -r '.[]')
for member in $MEMBERS; do
    echo "Processing peer: $member"
    MEMBER_JSON=$(echo $TEAM_JSON  | jq ".$member")
    
    cardanoVK=$(echo $MEMBER_JSON | jq '.["cardano-vk"]')
    cat << EOF >./$DIR/$member.cardano.vk
{
    "type": "PaymentVerificationKeyShelley_ed25519",
    "description": "Payment Verification Key",
    "cborHex": $cardanoVK
}
EOF

    hydraVK=$(echo $MEMBER_JSON | jq '.["hydra-vk"]')
    echo $hydraVK | tr -d '"' | base64 -d > ./$DIR/$member.hydra.vk
done
