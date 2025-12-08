import React from 'react';

interface ModalProps {
  isOpen: boolean;
  onClose: () => void;
  title?: string;
  children: React.ReactNode;
  maxWidth?: 'sm' | 'md' | 'lg' | 'xl';
}

const maxWidthStyles = {
  sm: 'max-w-sm',
  md: 'max-w-md',
  lg: 'max-w-lg',
  xl: 'max-w-xl',
};

export const Modal: React.FC<ModalProps> = ({
  isOpen,
  onClose,
  title,
  children,
  maxWidth = 'md',
}) => {
  if (!isOpen) return null;

  return (
    <div
      className="fixed inset-0 bg-black/80 flex items-center justify-center z-50 p-5"
      onClick={onClose}
    >
      <div
        className={`
          bg-dark-card border border-border rounded-xl
          w-full ${maxWidthStyles[maxWidth]} max-h-[85vh] overflow-y-auto
        `}
        onClick={e => e.stopPropagation()}
      >
        {title && (
          <div className="flex justify-between items-center px-6 py-4 border-b border-border">
            <h3 className="text-accent text-lg font-medium">{title}</h3>
            <button
              className="text-slate-500 text-2xl leading-none hover:text-slate-300 transition-colors"
              onClick={onClose}
            >
              ×
            </button>
          </div>
        )}
        <div className="p-6">
          {children}
        </div>
      </div>
    </div>
  );
};

