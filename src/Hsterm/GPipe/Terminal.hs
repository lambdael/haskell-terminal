{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface, BangPatterns #-}
-- | PTY 管理。
--
-- 擬似端末（PTY）を作成し、シェルプロセスを起動して
-- ハンドルを返す。ターミナル状態の更新もここで行う。
-- OpenGL に依存しない純粋な IO モジュール。
module Hsterm.GPipe.Terminal
  ( PtyHandle(..)
  , spawnShell
  , runTerminalReader
  , drainAvailable
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException(..), catch)
import Control.Monad (forever, forM_)
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.IORef
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.C.Types (CInt(..))
import System.Environment (getEnvironment)
import System.IO
import System.Posix.IO (fdToHandle, closeFd, dupTo, stdInput, stdOutput, stdError)
import System.Posix.Process (forkProcess, executeFile, createSession, getProcessID)
import System.Posix.Signals (signalProcess, Signal)
import System.Posix.Terminal hiding (TerminalState)
import System.Posix.Types (CPid, Fd)

import Terminal.Parser (parseANSI)
import Terminal.Terminal (applyActionsBatched, newTerminal, setSize)
import Terminal.Types

-- | TIOCSCTTY ioctl — スレーブ PTY を制御端末に設定する。
foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> CInt -> IO CInt

-- | TIOCSCTTY の ioctl 番号 (Linux)
tiocsctty :: CInt
tiocsctty = 0x540E

-- | PTY ハンドル。シェルプロセスの入出力と PID を保持する。
data PtyHandle = PtyHandle
  { ptyMaster :: !Handle   -- ^ マスター側ハンドル（入出力兼用）
  , ptyPid    :: !CPid     -- ^ シェルプロセスの PID
  , ptyFd     :: !Fd       -- ^ マスター側ファイルディスクリプタ（ioctl 用）
  }

-- | シェルプロセスを PTY 上で起動する。
--
-- forkProcess を使い、子プロセスで setsid + TIOCSCTTY を呼ぶことで
-- スレーブ PTY を制御端末として設定する。
-- これにより PTY の line discipline が \x03 を SIGINT に変換できる。
spawnShell :: IO PtyHandle
spawnShell = do
  rawenv <- getEnvironment
  let shell = fromMaybe "bash" $ lookup "SHELL" rawenv
      environment = [("TERM", "xterm")]
      env' = override rawenv environment

  (masterFd, slaveFd) <- openPseudoTerminal

  pid <- forkProcess $ do
    -- 子プロセス: マスター側は不要
    closeFd masterFd

    -- 新しいセッションを作成（制御端末なし状態）
    _ <- createSession

    -- スレーブ PTY を制御端末に設定
    let slaveRawFd = fromIntegral slaveFd :: CInt
    _ <- c_ioctl slaveRawFd tiocsctty 0

    -- stdin/stdout/stderr をスレーブ PTY にリダイレクト
    _ <- dupTo slaveFd stdInput
    _ <- dupTo slaveFd stdOutput
    _ <- dupTo slaveFd stdError
    -- 重複していない場合のみ閉じる
    if slaveFd /= stdInput && slaveFd /= stdOutput && slaveFd /= stdError
      then closeFd slaveFd
      else return ()

    -- シェルを exec
    executeFile shell True [] (Just env')

  -- 親プロセス: スレーブ側は不要
  closeFd slaveFd

  master <- fdToHandle masterFd
  hSetBuffering master NoBuffering

  pure PtyHandle
    { ptyMaster = master
    , ptyPid    = pid
    , ptyFd     = masterFd
    }

-- | PTY 出力を読み取ってターミナル状態を更新するスレッド。
-- dirty フラグを立てて、次の表示更新時にだけ再描画させる。
-- 全アクションを一括適用してから writeIORef するので、
-- 描画スレッドが中間状態を読むことはない。
runTerminalReader :: IORef Terminal -> IORef Bool -> Handle -> IO ()
runTerminalReader termRef dirty hOut = do
  -- UTF-8 マルチバイト文字が読み取り境界で切断される場合に備え、
  -- 未完成のバイト列を保持するバッファ。
  utf8buf <- newIORef BS.empty
  forever $ do
    chunk <- BS.hGetSome hOut 65536
    prevBytes <- readIORef utf8buf
    let allBytes = if BS.null prevBytes then chunk else BS.append prevBytes chunk
        -- UTF-8 デコード: 末尾の不完全なマルチバイトシーケンスを分離する
        (decoded, remainder) = decodeUtf8Partial allBytes
        str = T.unpack decoded
    writeIORef utf8buf remainder
    term <- readIORef termRef

    let Right (actions, leftover) = parseANSI $ inBuffer term ++ str
        !term' = applyActionsBatched term actions
    writeIORef termRef $ term' { inBuffer = leftover }

    writeIORef dirty True

-- | UTF-8 バイト列をデコードし、末尾の不完全なシーケンスを分離する。
-- 完全にデコードできた部分を Text で、残りの不完全バイトを ByteString で返す。
decodeUtf8Partial :: BS.ByteString -> (T.Text, BS.ByteString)
decodeUtf8Partial bs
  | BS.null bs = (T.empty, BS.empty)
  | otherwise =
      -- 末尾から不完全な UTF-8 シーケンスを探す（最大3バイト戻る）
      let len = BS.length bs
          -- 末尾の不完全バイト数を計算
          trailLen = incompleteUtf8Tail bs len
          (complete, trail) = BS.splitAt (len - trailLen) bs
      in (TE.decodeUtf8With (\_ _ -> Just '\xFFFD') complete, trail)

-- | ByteString の末尾にある不完全な UTF-8 シーケンスのバイト数を返す。
-- 完全なシーケンスしかない場合は 0 を返す。
incompleteUtf8Tail :: BS.ByteString -> Int -> Int
incompleteUtf8Tail bs len
  | len == 0 = 0
  | otherwise =
      -- 末尾から最大 3 バイト戻り、マルチバイト先頭を探す
      let check n
            | n > 3 || n > len = 0  -- 4バイト以上戻る必要はない
            | otherwise =
                let b = BS.index bs (len - n)
                in if b < 0x80
                   then 0  -- ASCII: 完全
                   else if b >= 0xC0  -- マルチバイト先頭バイト
                        then let expected
                                   | b < 0xE0 = 2
                                   | b < 0xF0 = 3
                                   | otherwise = 4
                             in if n < expected then n else 0
                        else check (n + 1)  -- 継続バイト (0x80-0xBF): さらに戻る
      in check 1

-- | Handle から読めるだけノンブロッキングで読み取る（UTF-8 デコード）
drainAvailable :: Handle -> IO String
drainAvailable h = do
  ready <- hReady h `catch` \(SomeException _) -> return False
  if ready
    then do
      bs <- BS.hGetNonBlocking h 65536
      let (decoded, _) = decodeUtf8Partial bs
      return (T.unpack decoded)
    else return []

-- ── helpers ──────────────────────────────────────────────

override :: [(String, String)] -> [(String, String)] -> [(String, String)]
override source rider =
  let pick key = case lookup key rider of
        Just v  -> (key, v)
        Nothing -> (key, fromMaybe "" (lookup key source))
      allKeys = map fst source ++ [k | (k, _) <- rider, lookup k source == Nothing]
  in  map pick allKeys
