/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import React, {type ReactNode} from 'react';
import CodeBlock from '@theme/CodeBlock';
import styles from './styles.module.css';

interface Props {
  children: ReactNode;
  minHeight: number;
}

export default function TerminalWindow({
  children,
  minHeight
}: Props): JSX.Element {
  const content = (typeof children === 'string') ? (<CodeBlock>{children}</CodeBlock>) : children;
  return (
    <div className={styles.terminalWindow} style={{minHeight}}>
      <div className={styles.terminalWindowHeader}>
        <div className={styles.buttons}>
          <span className={styles.dot} style={{background: '#f25f58'}} />
          <span className={styles.dot} style={{background: '#fbbe3c'}} />
          <span className={styles.dot} style={{background: '#58cb42'}} />
        </div>
      </div>
      <div className={styles.terminalWindowBody}>{content}</div>
    </div>
  );
}
