-- | IO Device for a Handle
module Hsterm.Device
where
import Data.Word
import GHC.IO.Buffer
import GHC.IO.Device
import  Control.Exception
ioe_unsupportedOperation :: IO a
ioe_unsupportedOperation = throwIO $userError "unsupportedOperation"


data Hasterm = Hasterm{
  echo :: Bool
  , keyInBuffer :: Buffer Word8
  , screenOutBuffer :: Buffer Word8
  }
instance IODevice Hasterm where
  -- | @ready dev write msecs@ returns 'True' if the device has data
  -- to read (if @write@ is 'False') or space to write new data (if
  -- @write@ is 'True').  @msecs@ specifies how long to wait, in
  -- milliseconds.
  -- 
  ready dev True msec = return True
  ready dev False msec = return False
  
  -- | closes the device.  Further operations on the device should
  -- produce exceptions.
  close a = return ()
  
  -- | returns 'True' if the device is a terminal or console.
  isTerminal _ = return True

  -- | returns 'True' if the device supports 'seek' operations.
  isSeekable _ = return False

  -- | seek to the specified position in the data.
  seek _ _ _ = ioe_unsupportedOperation

  -- | return the current position in the data.
  tell _ = ioe_unsupportedOperation

  -- | return the size of the data.
  getSize _ = ioe_unsupportedOperation

  -- | change the size of the data.
  setSize _ _ = ioe_unsupportedOperation

  -- | for terminal devices, changes whether characters are echoed on
  -- the device.
  setEcho dev b = return $ dev{echo = b}

  -- | returns the current echoing status.
  getEcho dev = return $ echo dev

  -- | some devices (e.g. terminals) support a "raw" mode where
  -- characters entered are immediately made available to the program.
  -- If available, this operations enables raw mode.
  setRaw dev b = return ()

  -- | returns the 'IODeviceType' corresponding to this device.
  devType _ = return Stream
  -- | duplicates the device, if possible.  The new device is expected
  -- to share a file pointer with the original device (like Unix @dup@).
  dup _ = ioe_unsupportedOperation

  -- | @dup2 source target@ replaces the target device with the source
  -- device.  The target device is closed first, if necessary, and then
  -- it is made into a duplicate of the first device (like Unix @dup2@).
  dup2 _ _ = ioe_unsupportedOperation

