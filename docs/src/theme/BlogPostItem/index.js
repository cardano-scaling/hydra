import React from 'react';
import BlogPostItem from '@theme-original/BlogPostItem';
import DocumentMetadata from '@site/src/components/DocumentMetadata';

export default function BlogPostItemWrapper(props) {
  return (
    <>
      <BlogPostItem {...props} />
      <DocumentMetadata />
    </>
  );
}
