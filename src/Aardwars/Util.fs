namespace Elm

open System.Threading
open System.Collections.Concurrent
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.Base
open Adaptify
open Aardvark.Rendering
open Aardvark.Application
open System

type Environment<'msg> =
    abstract Emit : #seq<'msg> -> unit
    abstract Window : IRenderWindow
    abstract Runtime : IRuntime
    abstract Shutdown : unit -> unit
    abstract StartTimer : interval : int * action : (unit -> unit) -> IDisposable

type App<'model, 'mmodel, 'message> =
    {
        initial : Environment<'message> -> 'model
        update : Environment<'message> -> 'model -> 'message -> 'model
        view : Environment<'message> -> 'mmodel -> ISg
        unpersist : Unpersist<'model, 'mmodel>
    }

module App =
    
    let inline create (initial : Environment<'message> -> 'model) (update : Environment<'message> -> 'model -> 'message -> 'model) (view : Environment<'message> -> 'mmodel -> ISg) =
        {
            initial = initial
            update = update
            view = view
            unpersist = Unpersist.instance
        }


    let run (window : IRenderWindow) (app : App<'model, 'mmodel, 'message>) =
        let messageQueue = new BlockingCollection<seq<'message>>()
        let mutable updateThread : Thread = null
        let threads = System.Collections.Concurrent.ConcurrentHashSet<IDisposable>()
        let env =
            { new Environment<'message> with
                member x.Emit(msgs : #seq<'message>) =
                    if not messageQueue.IsAddingCompleted then
                        messageQueue.Add (msgs :> seq<_>)
                member x.Window =
                    window
                member x.Runtime =
                    window.Runtime
                member x.Shutdown() =
                    messageQueue.CompleteAdding()
                    if not (isNull updateThread) then
                        updateThread.Join()
                    threads |> Seq.iter (fun d -> d.Dispose())

                member x.StartTimer(interval : int, action : unit -> unit) =
                    let timer = new MultimediaTimer.Trigger(interval)
                    let mutable running = true
                    let thread =
                        startThread <| fun () ->
                            while running do
                                timer.Wait()
                                action()
                    
                    let bla = 
                         { new IDisposable with
                            member x.Dispose() =    
                                running <- false
                                thread.Join()
                                timer.Dispose()
                        }   
                    threads.Add bla |> ignore
                    { new IDisposable with
                        member x.Dispose() =
                            threads.Remove bla |> ignore
                            bla.Dispose()
                    }
            }
        
        let mutable model = app.initial env
        let mmodel = app.unpersist.init model

        updateThread <-
            startThread <| fun () ->
                for msgs in messageQueue.GetConsumingEnumerable() do
                    transact (fun () ->
                        for msg in msgs do
                            model <- app.update env model msg

                        app.unpersist.update mmodel model
                    )


        let scene = app.view env mmodel
        let task = Sg.compile' window.Runtime window.FramebufferSignature false scene
        window.RenderTask <- task
        window.Run()
        env.Shutdown()

module Passes =
    let pass0 = (RenderPass.after "main" RenderPassOrder.Arbitrary RenderPass.main)
    let pass1 = (RenderPass.after "shots" RenderPassOrder.Arbitrary pass0)
    let pass2 = (RenderPass.after "weapon" RenderPassOrder.Arbitrary pass1)
    let pass3 = (RenderPass.after "text" RenderPassOrder.Arbitrary pass2)
                




