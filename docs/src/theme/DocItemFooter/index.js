import React from 'react';
import DocItemFooter from '@theme-original/DocItemFooter';
import DocumentMetadata from '@site/src/components/DocumentMetadata';

export default function DocItemFooterWrapper(props) {
  return (
    <>
      <DocumentMetadata />
      <DocItemFooter {...props} />
    </>
  );
}
