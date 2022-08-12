import React, { type ReactNode } from 'react';
import styles from './styles.module.css';

interface Props {
  lastUpdatedAt: string;
  commitHash: string;
  lastTranslatedAt?: string;
};

export default function DocumentMetadata({
  lastUpdatedAt,
  commitHash,
  lastTranslatedAt
}: Props): JSX.Element {

  const link =
    `https://github.com/input-output-hk/hydra-poc/commit/${commitHash}`;

  return (
    <div className={styles.block}>
      <i className={styles.info}>Last updated at: <b>{lastUpdatedAt}</b></i>
      <i className={styles.info}>Last commit: <a href={link}><b>{commitHash}</b></a></i>
      {
        lastTranslatedAt &&
        <i className={styles.info}>Last translated at: <b>{lastTranslatedAt}</b></i>
      }
    </div >
  );
}
