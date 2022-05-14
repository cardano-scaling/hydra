import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';
import {translate} from '@docusaurus/Translate';

const FeatureList = [
  {
    title: translate({
      id: "homepage.featureList.secure.title",
      message: "Secure",
      description: "Hydra heads' first highlight: security."
    }),
    src: require('@site/static/img/knight.png').default,
    description: translate({
      id: "homepage.featureList.secure.tagline",
      message: "Stay confident that participants of a Hydra Head can’t lose funds they’ve not explicitly agreed to spend.",
      description: "Tagline for the 'secure' property."
    }),
  },
  {
    title: translate({
      id: "homepage.featureList.fast.title",
      message: "Fast",
      description: "Hydra heads' second highlight: swiftness."
    }),
    src: require('@site/static/img/runner.png').default,
    description: translate({
      id: "homepage.featureList.fast.tagline",
      message: "Benefit from near-instant settlement for transactions executed inside a Hydra Head.",
      description: "Tagline for the 'fast' property."
    }),
  },
  {
    title: translate({
      id: "homepage.featureList.isomorphic.title",
      message: "Isomorphic",
      description: "Hydra heads' third highlight: isomorphism."
    }),
    src: require('@site/static/img/astro_kitten.png').default,
    description: translate({
      id: "homepage.featureList.isomorphic.tagline",
      message: "Harness robust capabilities, interfaces and safety of the Cardano main ledger.",
      description: "Tagline for the 'isomorphic' property."
    }),
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
