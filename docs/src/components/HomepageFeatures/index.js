import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';

const FeatureList = [
  {
    title: 'Secure',
    src: require('@site/static/img/knight.png').default,
    description: (
      <>
        Participants of a Hydra head can never lose funds they haven't explicitly agreed on spending.
      </>
    ),
  },
  {
    title: 'Fast',
    src: require('@site/static/img/runner.png').default,
    description: (
      <>
        Near-instant settlement for transactions executed inside a head network.
      </>
    ),
  },
  {
    title: 'Isomorphic',
    src: require('@site/static/img/astro_kitten.png').default,
    description: (
      <>
        Leverage capabilities, interfaces and safety of the underlying layer 1 ledger.
      </>
    ),
  },
];

function Feature({src, title, description}) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <img src={src} className={styles.featureSvg} />
      </div>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
