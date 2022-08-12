/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import React, { type ReactNode } from 'react';
import styles from './styles.module.css';

interface Props {
  lastUpdatedAt: string;
  authorInfo: string;
  lastTranslatedAt?: string;
};

export default function lastUpdatedAt({
  lastUpdatedAt,
  authorInfo,
  lastTranslatedAt
}: Props): JSX.Element {
  return (
    <div className={styles.block}>
      <i className={styles.info}>Last updated at: <b>{lastUpdatedAt}</b></i>
      <i className={styles.info}>Author: <b>{authorInfo}</b></i>
      {lastTranslatedAt && <i className={styles.info}>Last translated at: <b>{lastTranslatedAt}</b></i>}
    </div>
  );
}
