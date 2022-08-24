import React from 'react';
import EditThisPage from '@theme-original/EditThisPage';
import DocumentMetadata from '@site/src/components/DocumentMetadata';

export default function EditThisPageWrapper(props) {
  return (
    <>
      <DocumentMetadata />
      <EditThisPage {...props} />
    </>
  );
}
