import React, { useEffect, useId, useMemo, useRef, useState } from "react";
import { DraggableRenderer, inOrder, param, translate } from "dragology";
import { formatValue, fundsValue, operatorStatus, targets } from "./model.mjs";
import styles from "./styles.module.css";

const lane = (fund) => (["l1", "deposit"].includes(fund.place) ? "l1" : "l2");

function FundFace({
  name,
  ada,
  tokens,
  side,
  place,
  reservation,
  enabled,
  selected,
  onSelect,
}) {
  const label = reservation
    ? "reserved"
    : ["deposit", "closed"].includes(place)
    ? "locked"
    : !["l1", "l2"].includes(place)
    ? "pending"
    : null;
  const lines = [
    ada || !tokens ? { text: `₳${ada}`, style: styles.amount } : null,
    tokens
      ? {
          text: `${tokens} DEMO`,
          style: styles.tokenAmount,
        }
      : null,
    label ? { text: label, style: styles.pendingLabel } : null,
  ].filter(Boolean);
  return (
    <g
      role="button"
      tabIndex={enabled ? 0 : -1}
      aria-label={`${name}: ${formatValue(
        ada,
        tokens
      )} on ${side.toUpperCase()}${
        reservation ? `, ${reservation}` : label ? `, ${label}` : ""
      }`}
      aria-disabled={!enabled}
      aria-pressed={selected}
      onClick={() => onSelect(false)}
      onKeyDown={(event) => {
        if (event.key === "Enter" || event.key === " ") {
          event.preventDefault();
          onSelect(true);
        }
      }}
    >
      {reservation && <title>{reservation}</title>}
      <rect width={68} height={56} rx={8} />
      {lines.map((line, i) => (
        <text
          key={line.style}
          transform={translate(34, 28 + (i - (lines.length - 1) / 2) * 17)}
          textAnchor="middle"
          dominantBaseline="central"
          className={line.style}
        >
          {line.text}
        </text>
      ))}
    </g>
  );
}

function slots(state, width) {
  const gap = width < 500 ? 12 : 24;
  const column = (width - gap) / 2;
  const across = Math.max(1, Math.floor((column - 20) / 76));
  const header = width < 500 ? 60 : 38;
  let y = 0;
  const rows = state.people.map((person) => {
    const owned = state.funds.filter((fund) => fund.owner === person.id);
    const left = owned.filter((fund) => lane(fund) === "l1").length;
    const right = owned.length - left;
    const capacity = Math.max(left + (right ? 1 : 0), right + 1);
    const height = header + 8 + Math.ceil(capacity / across) * 64;
    const row = { person, y, height };
    y += height + 10;
    return row;
  });
  return { rows, column, across, gap, header, height: y };
}

export default function Board({
  state,
  disabled,
  selected,
  onSelect,
  onMove,
  reducedMotion,
}) {
  const container = useRef(null);
  const [width, setWidth] = useState(700);
  const prefix = useId().replace(/:/g, "");
  useEffect(() => {
    const observer = new ResizeObserver(([entry]) =>
      setWidth(Math.floor(entry.contentRect.width))
    );
    observer.observe(container.current);
    return () => observer.disconnect();
  }, []);
  const layout = useMemo(() => slots(state, width), [state, width]);
  const view = useMemo(() => ({ preview: null }), [state]);
  const movesFor = (fundId) => (disabled ? [] : targets(state, fundId));

  const draw = ({ state: viewState, d, draggedId }) => {
    const preview = viewState.preview;
    const displayFunds = state.funds.map((fund) =>
      preview?.fundId === fund.id
        ? { ...fund, owner: preview.target.owner, place: preview.target.layer }
        : fund
    );
    const activeFund = preview?.fundId || selected;
    const available = movesFor(activeFund);
    return (
      <g>
        {layout.rows.flatMap(({ person, y, height }) =>
          ["l1", "l2"].map((side, columnIndex) => {
            const x = columnIndex * (layout.column + layout.gap);
            const valid = available.some(
              (target) => target.owner === person.id && target.layer === side
            );
            const hovered =
              preview?.target.owner === person.id &&
              preview?.target.layer === side;
            const funds = state.funds.filter(
              (f) => f.owner === person.id && lane(f) === side
            );
            const amount = fundsValue(funds.filter((f) => f.place === side));
            const status =
              person.role === "operator"
                ? operatorStatus(state, person.id)
                : null;
            const label = `${person.name} · ${
              status
                ? side === "l2"
                  ? status.label
                  : "Head operator"
                : "Wallet user"
            }`;
            return (
              <g
                id={`${prefix}-cell-${side}-${person.id}`}
                transform={translate(x, y)}
              >
                <title>{label}</title>
                <rect
                  x={2}
                  y={2}
                  width={layout.column - 4}
                  height={height - 4}
                  rx={10}
                  className={styles.account}
                  data-destination={valid}
                  data-hovered={hovered}
                  data-account={`${side}-${person.id}`}
                  data-role={side === "l2" ? person.role : undefined}
                />
                <text
                  transform={translate(12, 25)}
                  className={styles.accountName}
                  aria-label={label}
                >
                  {person.name}
                  {side === "l2" && status?.marks}
                </text>
                <text
                  transform={translate(
                    width < 500 ? 12 : layout.column - 12,
                    width < 500 ? 45 : 25
                  )}
                  textAnchor={width < 500 ? "start" : "end"}
                  className={styles.balance}
                >
                  {amount}
                </text>
                {funds.length === 0 && (
                  <text
                    transform={translate(12, layout.header + 27)}
                    className={styles.empty}
                  >
                    {valid ? "Drop funds here" : "Empty"}
                  </text>
                )}
              </g>
            );
          })
        )}
        {displayFunds.map((fund) => {
          const side = lane(fund);
          const row = layout.rows.find((r) => r.person.id === fund.owner);
          const siblings = displayFunds.filter(
            (f) => f.owner === fund.owner && lane(f) === side
          );
          const index = siblings.findIndex((f) => f.id === fund.id);
          const floating = preview?.fundId === fund.id;
          const x =
            (side === "l1" ? 0 : layout.column + layout.gap) +
            (floating ? viewState.dropX : 12 + (index % layout.across) * 76);
          const y =
            row.y +
            (floating
              ? viewState.dropY
              : layout.header + Math.floor(index / layout.across) * 64);
          const options = movesFor(fund.id);
          const pending = !["l1", "l2"].includes(fund.place);
          const id = `${prefix}-${fund.id}`;
          const select = (keyboard) => {
            if (options.length) onSelect(fund.id, keyboard);
          };
          return (
            <g
              id={id}
              transform={translate(x, y)}
              data-fund={fund.id}
              data-place={fund.place}
              data-owner={fund.owner}
              data-enabled={options.length > 0}
              data-pending={pending}
              data-reserved={Boolean(fund.reservation)}
              data-selected={selected === fund.id}
              className={styles.fund}
              dragologyZIndex={draggedId === id ? "/1" : false}
              dragologyOnDrag={
                options.length
                  ? () =>
                      d
                        .closest([
                          viewState,
                          ...options.map((target) => {
                            const destination = layout.rows.find(
                              (r) => r.person.id === target.owner
                            );
                            return d.vary(
                              {
                                preview: { fundId: fund.id, target },
                                dropX: 12,
                                dropY: layout.header,
                              },
                              [param("dropX"), param("dropY")],
                              {
                                constraint: (next) => [
                                  ...inOrder([
                                    4,
                                    next.dropX,
                                    layout.column - 72,
                                  ]),
                                  ...inOrder([
                                    4,
                                    next.dropY,
                                    destination.height - 60,
                                  ]),
                                ],
                              }
                            );
                          }),
                        ])
                        .whenFar(viewState, { gap: 28 })
                        .withFloating()
                        .withDropTransition(reducedMotion ? 0 : 180)
                  : undefined
              }
            >
              <rect width={68} height={56} className={styles.grab} />
              <g dragologyOpaque>
                <FundFace
                  name={row.person.name}
                  ada={fund.ada}
                  tokens={fund.tokens}
                  side={side}
                  place={fund.place}
                  reservation={fund.reservation}
                  enabled={options.length > 0}
                  selected={selected === fund.id}
                  onSelect={select}
                />
              </g>
            </g>
          );
        })}
      </g>
    );
  };

  return (
    <div
      ref={container}
      className={styles.board}
      aria-label="Funds on L1 and L2"
    >
      <DraggableRenderer
        key={`${state.messages.length}-${width < 500}`}
        draggable={draw}
        state={view}
        width={width}
        height={layout.height}
        dragThreshold={5}
        onDropState={(next) => {
          if (!disabled && next.preview)
            onMove(next.preview.fundId, next.preview.target);
        }}
      />
    </div>
  );
}
