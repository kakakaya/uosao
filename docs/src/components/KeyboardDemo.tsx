import { useState } from 'react';
import Keyboard from 'react-simple-keyboard';
import 'react-simple-keyboard/build/css/index.css';
import './KeyboardDemo.css';

// QWERTY 配列
const qwertyLayout = {
  default: [
    'q w e r t y u i o p',
    'a s d f g h j k l ;',
    'z x c v b n m , . /',
  ],
  shift: [
    'Q W E R T Y U I O P',
    'A S D F G H J K L :',
    'Z X C V B N M < > ?',
  ]
};

// Dvorak 配列
const dvorakLayout = {
  default: [
    '\' , . p y f g c r l',
    'a o e u i d h t n s',
    '; q j k x b m w v z',
  ],
  shift: [
    '" < > P Y F G C R L ?',
    'A O E U I D H T N S',
    ': Q J K X B M W V Z',
  ]
};

export default function KeyboardDemo() {
  const [input, setInput] = useState('');
  const [layoutName, setLayoutName] = useState('default');
  const [layoutType, setLayoutType] = useState<'qwerty' | 'dvorak'>('qwerty');

  const onChange = (newInput: string) => {
    setInput(newInput);
  };

  const onKeyPress = (button: string) => {
    if (button === '{shift}' || button === '{lock}') {
      setLayoutName(layoutName === 'default' ? 'shift' : 'default');
    }

    // "d" キーで Dvorak に切り替え
    if (button === 'd' || button === 'D') {
      setLayoutType('dvorak');
    }
    // "q" キーで QWERTY に戻す
    if (button === 'q' || button === 'Q') {
      setLayoutType('qwerty');
    }
  };

  const currentLayout = layoutType === 'dvorak' ? dvorakLayout : qwertyLayout;

  return (
    <div>
      <div style={{ marginBottom: '10px' }}>
        現在の配列: <strong>{layoutType.toUpperCase()}</strong>
        （d: Dvorak / q: QWERTY）
      </div>
      <input
        value={input}
        placeholder="キーボードで入力..."
        onChange={(e) => setInput(e.target.value)}
        style={{ width: '100%', padding: '10px', marginBottom: '10px', fontSize: '16px' }}
      />
      <Keyboard
        layoutName={layoutName}
        layout={currentLayout}
        onChange={onChange}
        onKeyPress={onKeyPress}
        theme="hg-theme-default ortho-keyboard"
      />
    </div>
  );
}
