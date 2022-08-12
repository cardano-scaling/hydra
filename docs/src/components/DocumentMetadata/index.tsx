import React, { type ReactNode } from 'react';
import styles from './styles.module.css';

interface Props {
  lastUpdatedAt: string;
  lastTranslatedAt?: string;
};

export default function DocumentMetadata({
  lastUpdatedAt,
  lastTranslatedAt
}: Props): JSX.Element {
  return (
    <div className={styles.block}>
      <i className={styles.info}>Last updated at: <b>{lastUpdatedAt}</b></i>
      {lastTranslatedAt &&
        <i className={styles.info}>Last translated at: <b>{lastTranslatedAt}</b></i>
      }
    </div>
  );
}
