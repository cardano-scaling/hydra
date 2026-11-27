export const CHIP_VALUE = 10;
export const operators = (state) =>
  state.people.filter((p) => p.role === "operator");
export const person = (state, id) => state.people.find((p) => p.id === id);
export const targets = (state, fundId) => state.targets[fundId] || [];
