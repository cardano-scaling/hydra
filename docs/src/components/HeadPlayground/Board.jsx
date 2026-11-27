import React, { useEffect, useId, useMemo, useRef, useState } from "react";
import { DraggableRenderer, inOrder, param, translate } from "dragology";
import { CHIP_VALUE, targets } from "./model.mjs";
import styles from "./styles.module.css";

const lane = (fund) => (["l1", "deposit"].includes(fund.place) ? "l1" : "l2");

function FundFace({
  name,
  side,
  place,
  reservation,
  enabled,
  selected,
  onSelect,
}) {
  const label = reservation
    ? "reserved"
    : place === "deposit"
    ? "locked"
    : !["l1", "l2"].includes(place)
    ? "pending"
    : null;
  return (
    <g
      role="button"
      tabIndex={enabled ? 0 : -1}
      aria-label={`${name}: ${CHIP_VALUE} ADA on ${side.toUpperCase()}${
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
      <rect width={56} height={44} rx={8} />
      <text
        transform={translate(28, label ? 18 : 28)}
        textAnchor="middle"
        className={styles.amount}
      >
        ₳{CHIP_VALUE}
      </text>
      {label && (
        <text
          transform={translate(28, 36)}
          textAnchor="middle"
          className={styles.pendingLabel}
        >
          {label}
        </text>
      )}
    </g>
  );
}

function slots(state, width) {
  const gap = width < 500 ? 12 : 24;
  const column = (width - gap) / 2;
  const across = Math.max(1, Math.floor((column - 20) / 64));
  let y = 0;
  const rows = state.people.map((person) => {
    const owned = state.funds.filter((fund) => fund.owner === person.id);
    const left = owned.filter((fund) => lane(fund) === "l1").length;
    const right = owned.length - left;
    const capacity = Math.max(left + (right ? 1 : 0), right + 1);
    const height = 42 + Math.ceil(capacity / across) * 52;
    const row = { person, y, height };
    y += height + 10;
    return row;
  });
  return { rows, column, across, gap, height: y };
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
            const amount =
              funds.filter((f) => f.place === side).length * CHIP_VALUE;
            return (
              <g
                id={`${prefix}-cell-${side}-${person.id}`}
                transform={translate(x, y)}
              >
                <rect
                  width={layout.column}
                  height={height}
                  rx={10}
                  className={styles.account}
                  data-destination={valid}
                  data-hovered={hovered}
                  data-account={`${side}-${person.id}`}
                />
                <text
                  transform={translate(12, 25)}
                  className={styles.accountName}
                >
                  {person.name}
                </text>
                <text
                  transform={translate(layout.column - 12, 25)}
                  textAnchor="end"
                  className={styles.balance}
                >
                  {amount} ADA
                </text>
                {funds.length === 0 && (
                  <text transform={translate(12, 65)} className={styles.empty}>
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
            (floating ? viewState.dropX : 12 + (index % layout.across) * 64);
          const y =
            row.y +
            (floating
              ? viewState.dropY
              : 38 + Math.floor(index / layout.across) * 52);
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
                                dropY: 38,
                              },
                              [param("dropX"), param("dropY")],
                              {
                                constraint: (next) => [
                                  ...inOrder([
                                    4,
                                    next.dropX,
                                    layout.column - 60,
                                  ]),
                                  ...inOrder([
                                    4,
                                    next.dropY,
                                    destination.height - 48,
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
              <rect width={56} height={44} className={styles.grab} />
              <g dragologyOpaque>
                <FundFace
                  name={row.person.name}
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
        key={state.messages.length}
        draggable={draw}
        state={view}
        width={width}
        height={layout.height}
        dragThreshold={5}
        onDropState={(next) => {
          if (next.preview) onMove(next.preview.fundId, next.preview.target);
        }}
      />
    </div>
  );
}
