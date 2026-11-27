export const formatValue = (ada, tokens = 0) =>
  [ada || !tokens ? `${ada} ADA` : null, tokens ? `${tokens} DEMO` : null]
    .filter(Boolean)
    .join(" + ");
export const fundsValue = (funds) =>
  formatValue(
    funds.reduce((total, fund) => total + fund.ada, 0),
    funds.reduce((total, fund) => total + fund.tokens, 0)
  );
export const operators = (state) =>
  state.people.filter((p) => p.role === "operator");
export const person = (state, id) => state.people.find((p) => p.id === id);
export const targets = (state, fundId) => state.targets[fundId] || [];

export function operatorStatus(state, id) {
  const round = state.signing;
  const leader = round?.leader === id;
  const signed = round?.signers.includes(id) || false;
  const collecting = round && round.status !== "next";
  const receipt = signed ? "signature received" : "awaiting signature delivery";
  return {
    leader,
    signed,
    marks: `${leader ? " ★" : ""}${collecting ? (signed ? " ✓" : " ○") : ""}`,
    label: round
      ? `Snapshot ${round.number}${leader ? " leader" : ""}: ${
          collecting ? receipt : "next round"
        }`
      : "Head operator",
  };
}
