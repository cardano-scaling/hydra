import React from "react";

export default function FooterLayout({ links, logo, description, copyright }) {
  return (
    <footer className="footer bg-glow">
      <div className="pageContainer footer__inner">
        <div className="footer__top">
          <div className="footer__brand">
            {logo}
            {description && (
              <p className="footer__description">{description}</p>
            )}
          </div>
          {links}
        </div>
        {copyright}
      </div>
    </footer>
  );
}
