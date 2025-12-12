import React from 'react';

interface InputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  label?: string;
}

export const Input: React.FC<InputProps> = ({ label, className = '', ...props }) => {
  return (
    <div className="flex flex-col gap-1.5">
      {label && (
        <label className="text-sm text-slate-400">{label}</label>
      )}
      <input
        className={`
          flex-1 bg-dark-input border border-border rounded-md
          px-3 py-2.5 text-slate-200 text-sm font-mono
          transition-colors duration-200
          focus:outline-none focus:border-accent
          disabled:bg-dark-bg disabled:text-slate-500
          ${className}
        `}
        {...props}
      />
    </div>
  );
};

interface TextAreaProps extends React.TextareaHTMLAttributes<HTMLTextAreaElement> {
  label?: string;
}

export const TextArea: React.FC<TextAreaProps> = ({ label, className = '', ...props }) => {
  return (
    <div className="flex flex-col gap-1.5">
      {label && (
        <label className="text-sm text-slate-400">{label}</label>
      )}
      <textarea
        className={`
          w-full bg-dark-input border border-border rounded-md
          px-3 py-2.5 text-slate-200 text-sm font-mono
          resize-y transition-colors duration-200
          focus:outline-none focus:border-accent
          ${className}
        `}
        {...props}
      />
    </div>
  );
};

interface FileInputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  label?: string;
}

export const FileInput: React.FC<FileInputProps> = ({ label, className = '', ...props }) => {
  return (
    <div className="flex flex-col gap-1.5">
      {label && (
        <label className="text-sm text-slate-400">{label}</label>
      )}
      <input
        type="file"
        className={`
          text-slate-400 text-sm
          file:mr-3 file:py-1.5 file:px-3
          file:rounded file:border file:border-border
          file:text-sm file:font-medium
          file:bg-dark-hover file:text-slate-200
          file:cursor-pointer file:transition-colors
          hover:file:bg-border
          ${className}
        `}
        {...props}
      />
    </div>
  );
};

