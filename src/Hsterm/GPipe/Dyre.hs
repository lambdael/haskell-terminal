{-# LANGUAGE DeriveGeneric #-}
-- | Dyre 統合 — リコンパイル＋exec によるホットリロード。
--
-- xmonad のように、ユーザが Haskell で設定を書き、
-- @Ctrl+Shift+R@ でリコンパイル＋再起動できる。
-- PTY や端末状態は exec 越しに引き継がれる。
module Hsterm.GPipe.Dyre
  ( -- * ユーザ向けエントリーポイント
    hsterm
    -- * Dyre パラメータ
  , dyreParams
    -- * 状態の保存と復元
  , ResumeState(..)
  , restoreResumeState
    -- * リロードアクション
  , requestReload
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import System.Directory (getXdgDirectory, XdgDirectory(..), createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)
import System.Posix.IO (setFdOption, FdOption(..))

import qualified Config.Dyre as Dyre
import Config.Dyre.Relaunch (relaunchWithBinaryState, restoreBinaryState)

import Hsterm.GPipe.Config (TerminalConfig(..))
import Hsterm.GPipe.Monad
import Terminal.Types (Terminal)

-- ── exec 越しに引き継ぐ状態 ─────────────────────────────

-- | exec 越しに引き継ぐターミナル状態。
--
-- PTY fd は 'FD_CLOEXEC' を解除して exec を生き残らせ、
-- fd 番号と PID を整数として保存する。
-- @Terminal@ は Phase 3 で 'Binary' インスタンスを導出済み。
data ResumeState = ResumeState
  { rsTerm         :: !Terminal    -- ^ ターミナル画面・カーソル・属性の全状態
  , rsPtyFdNum     :: !Int         -- ^ PTY マスター fd 番号
  , rsPtyPidNum    :: !Int         -- ^ シェルプロセスの PID
  , rsScrollOffset :: !Int         -- ^ スクロールバックのオフセット
  } deriving (Generic)

instance Binary ResumeState

-- ── Dyre パラメータ ──────────────────────────────────────

-- | Dyre の設定パラメータを構築する。
--
-- @realMainFn@ にはターミナルの実メイン関数（@runGPipeTerminal@）を渡す。
-- 循環依存を避けるため、関数パラメータとして受け取る。
dyreParams :: (TerminalConfig -> IO ()) -> Dyre.Params TerminalConfig ()
dyreParams realMainFn = (Dyre.newParams "haskell-terminal" realMainFn showError)
  { Dyre.configDir  = Just (getXdgDirectory XdgConfig "haskell-terminal")
  , Dyre.cacheDir   = Just (getXdgDirectory XdgCache "haskell-terminal")
  , Dyre.statusOut  = hPutStrLn stderr
  }
  where
    showError cfg msg = cfg { tcErrorMsg = Just msg }

-- | Dyre を使ってターミナルを起動する。
--
-- ユーザの @config.hs@ では:
--
-- @
-- import Hsterm.GPipe.Dyre (hsterm)
-- import Hsterm.GPipe.Config (defaultConfig, TerminalConfig(..))
--
-- main = hsterm $ defaultConfig { tcFontSize = 32 }
-- @
hsterm :: (TerminalConfig -> IO ()) -> TerminalConfig -> IO ()
hsterm realMainFn cfg = do
  cacheDir <- getXdgDirectory XdgCache "haskell-terminal"
  createDirectoryIfMissing True cacheDir
  Dyre.wrapMain (dyreParams realMainFn) cfg

-- ── 状態の復元 ───────────────────────────────────────────

-- | Dyre リロードからの復帰時に保存された状態を復元する。
-- リロードでなければ 'Nothing' を返す。
restoreResumeState :: IO (Maybe ResumeState)
restoreResumeState = restoreBinaryState Nothing

-- ── リロードアクション ───────────────────────────────────

-- | ターミナルのリロードを要求する。
--
-- PTY fd の @FD_CLOEXEC@ を解除し、ターミナル状態を
-- 一時ファイルにシリアライズしてから @exec@ で再起動する。
-- この関数から戻ることはない（exec で置き換わるため）。
requestReload :: HstermM ()
requestReload = do
  term <- getTerminal
  fd <- asks hePtyFd
  pid <- asks hePtyPid
  scrollOff <- getScrollOffset
  liftIO $ do
    -- FD_CLOEXEC を解除して exec 越しに fd を引き継ぐ
    setFdOption fd CloseOnExec False
    -- 状態を保存して再起動
    let state = ResumeState
          { rsTerm         = term
          , rsPtyFdNum     = fromIntegral fd
          , rsPtyPidNum    = fromIntegral pid
          , rsScrollOffset = scrollOff
          }
    relaunchWithBinaryState (Just state) Nothing
