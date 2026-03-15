-- | ターミナルアクション用モナド。
--
-- キーバインドやフックのアクション実行に使う薄いラッパー。
-- @ReaderT HstermEnv IO@ で、ターミナル状態・PTY・dirty フラグ等にアクセスできる。
module Hsterm.GPipe.Monad
  ( -- * モナド
    HstermM
  , runHstermM
    -- * 環境
  , HstermEnv(..)
    -- * 選択
  , Selection(..)
  , selectionRange
    -- * プリミティブ操作
  , sendPty
  , getTerminal
  , modifyTerminal
  , setDirty
  , getScrollOffset
  , setScrollOffset
  , modifyScrollOffset
  , getSelection
  , setSelection
  , requestCopy
  , requestPaste
  , exitTerminal
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Data.Char (ord)
import Data.IORef
import qualified Data.Sequence as Seq
import System.Exit (exitSuccess)
import System.IO (hPutStr, hFlush, stderr, Handle)
import System.Posix.Signals (signalProcess, sigINT)
import System.Posix.Types (CPid, Fd)

import Hsterm.GPipe.Terminal (PtyHandle(..))
import Terminal.Types (Terminal(..))

-- ── 選択状態 ─────────────────────────────────────────────

-- | マウスによるテキスト選択の状態。
-- セル座標は 1-indexed の @(行, 列)@。
data Selection = Selection
  { selAnchor  :: !(Int, Int)  -- ^ ドラッグ開始位置
  , selCurrent :: !(Int, Int)  -- ^ 現在のドラッグ位置
  } deriving (Show, Eq)

-- | 選択範囲を正規化して @(開始, 終了)@ を返す（開始 <= 終了）。
selectionRange :: Selection -> ((Int, Int), (Int, Int))
selectionRange (Selection a b)
  | a <= b    = (a, b)
  | otherwise = (b, a)

-- ── 環境 ─────────────────────────────────────────────────

-- | ターミナルアクションの実行環境。
data HstermEnv = HstermEnv
  { heTermRef         :: !(IORef Terminal)
  , hePtyMaster       :: !Handle          -- ^ PTY マスターハンドル（書き込み用）
  , hePtyFd           :: !Fd              -- ^ PTY マスターの生 fd（ioctl / FD_CLOEXEC 用）
  , hePtyPid          :: !CPid            -- ^ シェルプロセスの PID
  , heDirty           :: !(IORef Bool)
  , heScrollOffsetRef :: !(IORef Int)
  , heSelectionRef    :: !(IORef (Maybe Selection))
  , heClipboardWrite  :: !(IORef (Maybe String))
  , hePasteRequest    :: !(IORef Bool)
  }

-- | キーバインドやフックで使うアクションモナド。
type HstermM = ReaderT HstermEnv IO

-- | HstermM アクションを実行する。
runHstermM :: HstermEnv -> HstermM a -> IO a
runHstermM env m = runReaderT m env

-- ── プリミティブ操作 ─────────────────────────────────────

-- | PTY にバイト列を送信する。
sendPty :: String -> HstermM ()
sendPty s = do
  h <- asks hePtyMaster
  liftIO $ do
    hPutStr stderr $ "sendPty: " ++ show (map ord s) ++ "\n"
    hPutStr h s
    hFlush h

-- | 現在のターミナル状態を読み取る。
getTerminal :: HstermM Terminal
getTerminal = asks heTermRef >>= liftIO . readIORef

-- | ターミナル状態を変更する。
modifyTerminal :: (Terminal -> Terminal) -> HstermM ()
modifyTerminal f = do
  ref <- asks heTermRef
  liftIO $ modifyIORef' ref f

-- | dirty フラグを立てる（次フレームで再描画）。
setDirty :: HstermM ()
setDirty = asks heDirty >>= \ref -> liftIO $ writeIORef ref True

-- | スクロールオフセットを取得する。
getScrollOffset :: HstermM Int
getScrollOffset = asks heScrollOffsetRef >>= liftIO . readIORef

-- | スクロールオフセットを設定する。
setScrollOffset :: Int -> HstermM ()
setScrollOffset n = asks heScrollOffsetRef >>= \ref -> liftIO $ writeIORef ref n

-- | スクロールオフセットを変更する。
modifyScrollOffset :: (Int -> Int) -> HstermM ()
modifyScrollOffset f = asks heScrollOffsetRef >>= \ref -> liftIO $ modifyIORef' ref f

-- | 選択範囲を取得する。
getSelection :: HstermM (Maybe Selection)
getSelection = asks heSelectionRef >>= liftIO . readIORef

-- | 選択範囲を設定する。
setSelection :: Maybe Selection -> HstermM ()
setSelection sel = asks heSelectionRef >>= \ref -> liftIO $ writeIORef ref sel

-- | クリップボードへのコピーを要求する。
-- 実際のクリップボード操作は ContextT ループ内で行われる。
requestCopy :: String -> HstermM ()
requestCopy txt = asks heClipboardWrite >>= \ref -> liftIO $ writeIORef ref (Just txt)

-- | クリップボードからのペーストを要求する。
requestPaste :: HstermM ()
requestPaste = asks hePasteRequest >>= \ref -> liftIO $ writeIORef ref True

-- | ターミナルを終了する（シェルプロセスに SIGINT を送信して exit）。
exitTerminal :: HstermM ()
exitTerminal = do
  pid <- asks hePtyPid
  liftIO $ do
    signalProcess sigINT pid
    exitSuccess
